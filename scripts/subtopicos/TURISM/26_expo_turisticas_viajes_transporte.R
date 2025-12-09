# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "expo_turisticas_viajes_transporte.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

df_output <- df_imf %>% 
  dplyr::filter(INDICATOR %in% c("SD", "SDZ"), 
                COUNTRY == "ARG",
                BOP_ACCOUNTING_ENTRY == "CD_T") %>% 
  mutate(expo_turisticas = OBS_VALUE / 1000000, 
         indicador = ifelse(INDICATOR == "SD", "Viajes", "Viajes y transporte")) %>% 
  select(
    anio = TIME_PERIOD,
    indicador, 
    expo_turisticas
  ) 



