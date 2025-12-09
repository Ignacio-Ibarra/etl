# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "arribos_turisticos_internacionales_por_regiones.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R491C315'
fuente2 <- 'R476C0'

df_owid <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_unwto_manual <- argendataR::get_raw_path(fuente2) %>% 
  read.csv(.)


geo_codes <- c(
  "Europe" = "ZEUR",
  "Americas" = "X21",
  "Asia and the Pacific" = "APA",
  "Asia & Pacific" = "APA",
  "Middle East" = "DESHUM_AHDI.MEA",
  "Africa" = "X06"
)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_unwto_complete <- df_unwto_manual %>% 
  dplyr::filter(is.na(m49_code), country != "World") %>% 
  mutate(geocodigoFundar = geo_codes[country]) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio = year, geocodigoFundar, geonombreFundar, arribos_turisticos_internacionales_millon = int_tourist_arraivals_million)

df_owid_complete <- df_owid %>% 
  mutate(geocodigoFundar = geo_codes[regiones]) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  mutate(arribos_turisticos_internacionales_millon = arribos_turisticos_internacionales/1000000) %>% 
  select(anio, geocodigoFundar, geonombreFundar, arribos_turisticos_internacionales_millon)
  
  
df_output <- bind_rows(
  
  df_unwto_complete,
  df_owid_complete %>% 
    dplyr::filter(anio<2016)
  
)

