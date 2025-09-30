# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "ratio_centralizacion_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R420C270' # OECD Fiscal Decentralisation Database. Consolidated government expenditure as percentage of total general government expenditure (consolidated) [Table 5: 1970 - 2023] - Consolidated government expenditure, National currency in millions at current prices
fuente2 <- 'R325C200' # Base de datos 1980-2023. Gasto público consolidado - Febrero 2025 - Países
fuente3 <- 'R326C201' # Base de datos 1980-2023. Gasto público nacional - Febrero 2025 - Países

df_oecd <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_gpc <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_gpn <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_oecd_centralization <- df_oecd %>% 
  drop_na(valor) %>% 
  group_by(anio, iso3) %>%
  dplyr::filter(n()>1) %>% 
  ungroup() %>% 
  group_by(anio, iso3) %>%
  mutate(share_gasto = 100 * valor /sum(valor, na.rm=T)) %>% 
  ungroup() %>% 
  dplyr::filter(nivel_gobierno == "Central") %>% 
  drop_na(share_gasto) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  select(iso3, pais_nombre, anio, share_gasto)


df_arg_centralization <- df_gpn %>% 
  dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL") %>% 
  select(anio, gpn = valores) %>% 
  left_join(df_gpc %>% 
              dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL") %>% 
              select(anio, gpc = valores), join_by(anio)) %>% 
  mutate(share_gasto = 100 * gpn / gpc,
         iso3 = "ARG",
         pais_nombre = "Argentina") %>% 
  dplyr::filter(anio == max(anio)) %>% 
  select(iso3, pais_nombre, anio, share_gasto)


df_output <- bind_rows(df_oecd_centralization, df_arg_centralization) %>% 
  arrange(-share_gasto) %>% 
  rename(geocodigoFundar = iso3, geonombreFundar = pais_nombre, ratio_centralizacion = share_gasto)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(geocodigoFundar = cod_pais, geonombreFundar = pais)

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar")
)


armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("anio", 
                      "geocodigoFundar",
                      "geonombreFundar",
                      "ratio_centralizacion"),
  descripcion = c("Año de referencia",
                  "Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país",
                  "Participación del gobierno central en el gasto público consolidado")
)


descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("geocodigoFundar"),
    descripcion_columnas = descripcion,
    unidades = list("ratio_centralizacion" = "porcentaje")
  )
 


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
