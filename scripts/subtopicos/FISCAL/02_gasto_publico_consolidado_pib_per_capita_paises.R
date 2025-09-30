# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_pib_per_capita_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R424C272'
fuente2 <- 'R325C200'
fuente3 <- 'R126C0'


df_imf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_mecon <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_wb <- argendataR::get_raw_path(fuente3) %>% 
  readr::read_csv(.) %>% 
  select(iso3 = iso3c, anio = year, gdp_pc_ppp_kd = `NY.GDP.PCAP.PP.KD`)


df_arg <- df_mecon %>% 
  dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL") %>%
  mutate(iso3 = 'ARG', 
         pais_nombre = 'Argentina') %>% 
  select(iso3, pais_nombre, anio, gasto_pub_gdp = valores)


df_gasto <- df_imf %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  select(iso3, pais_nombre, anio, gasto_pub_gdp = exp) %>% 
  bind_rows(df_arg) 


geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  dplyr::filter(nivel_agregacion == 'pais') %>% 
  mutate(region = case_when(
    continente_fundar == "América del Norte, Central y el Caribe" ~ intermediate_region_unsd,
    TRUE ~ continente_fundar
  )) %>% 
  select(iso3 = codigo_fundar, region)



df_output <- df_wb %>% 
  inner_join(df_gasto, join_by(anio, iso3)) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  drop_na(gasto_pub_gdp) %>% 
  drop_na(gdp_pc_ppp_kd) %>%
  left_join(geonomenclador, join_by(iso3)) %>%  
  rename(`Gasto público consolidado como porcentaje del PIB` = gasto_pub_gdp, `PIB per cápita PPP (en dólares internacionales constantes de 2021)` =  gdp_pc_ppp_kd) %>% 
  pivot_longer(., !all_of(c('anio', 'iso3', 'pais_nombre', 'region')),
               names_to = 'variable',
               values_to = 'valor') %>% 
  rename(geocodigoFundar = iso3, geonombreFundar = pais_nombre)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  rename(geocodigoFundar = cod_pais, 
         geonombreFundar = pais,
         `Gasto público consolidado como porcentaje del PIB` = gasto_publico_porcentaje_del_pib, 
         `PIB per cápita PPP (en dólares internacionales constantes de 2021)` =  pib_per_capita_ppp) %>%
  pivot_longer(., !all_of(c('geocodigoFundar', 'geonombreFundar')),
               names_to = 'variable',
               values_to = 'valor') 
  


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar", "variable")
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
                      "region",
                      "variable",
                      "valor"),
  descripcion = c("Año de referencia",
                  "Códigos de país ISO 3166 - alfa 3",
                  "Nombre de país de referencia",
                  "Región de países de referencia",
                  "Indica si la variable observada es `Gasto público consolidado como porcentaje del PIB` o `PIB per cápita PPP (en dólares internacionales constantes de 2021)`",
                  "Valor observado")
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
    pk = c("geocodigoFundar", "variable"),
    descripcion_columnas = descripcion,
    unidades = list("valor" = "unidades")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")

