# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "ratio_turismo_emisivo_impo_comparado.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


df_output <- df_imf %>% 
dplyr::filter(BOP_ACCOUNTING_ENTRY == "DB_T", INDICATOR %in% c("GS", "SD")) %>% 
  select(geocodigoFundar = COUNTRY,
         anio = TIME_PERIOD,
         valor = OBS_VALUE,
         indicador = INDICATOR) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  dplyr::filter(anio %in% 2016:max(anio)) %>% 
  group_by(geocodigoFundar, geonombreFundar, indicador) %>% 
  summarise(
    valor = sum(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = c(geocodigoFundar, geonombreFundar),
              names_from = indicador, values_from = valor) %>% 
  mutate(ratio_emisivo_ponderado = 100 * SD/ GS) %>% 
  select(geocodigoFundar, geonombreFundar, ratio_emisivo_ponderado) %>% 
  drop_na(ratio_emisivo_ponderado) %>% 
  drop_na(geonombreFundar) 
  

