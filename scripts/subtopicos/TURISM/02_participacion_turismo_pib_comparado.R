# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "participacion_turismo_pib_comparado.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R469C305'

df_unwto <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  dplyr::filter(!is.na(m49_code_unsd)) %>% 
  select(m49_code = m49_code_unsd, 
         geocodigoFundar = codigo_fundar, 
         geonombreFundar = desc_fundar)


df_output <- df_unwto %>% 
  left_join(geonomenclador, join_by(geo_area_code == m49_code)) %>% 
  select(anio = time_period, geocodigoFundar, geonombreFundar, share_tourism_gdp = value)

