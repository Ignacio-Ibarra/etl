# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_promedio_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R424C272'
fuente2 <- 'R325C200'


df_imf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_mecon <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_arg <- df_mecon %>% 
  dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL", anio>=2014) %>% 
  summarise(gasto_pub_gdp = mean(valores, na.rm=T)) %>% 
  mutate(iso3 = 'ARG', 
         pais_nombre = 'Argentina', 
         fuente = "MECON") %>% 
  select(iso3, pais_nombre, gasto_pub_gdp, fuente)


df_output <- df_imf %>% 
  dplyr::filter(anio>=2014) %>% 
  group_by(iso3, pais_nombre) %>% 
  summarise(
    gasto_pub_gdp = mean(exp, na.rm = T)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  mutate(fuente = "FMI") %>% 
  bind_rows(df_arg) %>% 
  arrange(-gasto_pub_gdp) %>% 
  select(geocodigoFundar = iso3, geonombreFundar = pais_nombre, gasto_publico_promedio = gasto_pub_gdp, fuente)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(geocodigoFundar = codigo_pais, geonombreFundar = pais)


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
  variable_nombre = c("geocodigoFundar", 
                      "geonombreFundar",
                      "fuente"),
  descripcion = c("Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país",
                  "Fuente de información utilizada")
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
    unidades = list("gasto_publico_promedio" = "Gasto público consolidado promedio del periodo 2014 a la fecha (en porcentaje del PIB)")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
