# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "participacion_argentina_turismo_receptivo.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'
fuente2 <- 'R492C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

df_unsd_country_codes <- argendataR::get_raw_path(fuente2) %>% 
  read.csv() %>% 
  select(geocodigoFundar = `ISO.alpha3.Code`, region = `Region.Name`) %>% 
  dplyr::filter(region !="")


df_output <- df_imf %>% 
  dplyr::filter(INDICATOR == "SD", BOP_ACCOUNTING_ENTRY == "CD_T") %>% 
  mutate(expo_turisticas = OBS_VALUE/1000000000) %>% 
  left_join(df_unsd_country_codes, join_by(COUNTRY == geocodigoFundar)) %>% 
  dplyr::filter(!is.na(region)) %>% 
  select(
    anio = TIME_PERIOD,
    region,
    expo_turisticas
  ) %>%
  group_by(anio, region) %>% 
  summarise(
    expo_turisticas = sum(expo_turisticas, na.rm = T)
  )
  

