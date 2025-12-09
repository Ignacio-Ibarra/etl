# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "exportaciones_turisticas_comparada.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


df_output <- df_imf %>% 
  dplyr::filter(INDICATOR == "SD", BOP_ACCOUNTING_ENTRY == "CD_T") %>% 
  select(
    anio = TIME_PERIOD,
    geocodigoFundar = COUNTRY, 
    expo_turisticas = OBS_VALUE
  ) %>%
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, expo_turisticas) %>% 
  mutate(expo_turisticas = expo_turisticas/1000000)

