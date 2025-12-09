# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "balanza_comercial_turistica.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()


df_output <- df_imf %>% 
  dplyr::filter(COUNTRY == "ARG", INDICATOR == "SD") %>% 
  select(
    anio = TIME_PERIOD,
    flujo = BOP_ACCOUNTING_ENTRY,
    valor = OBS_VALUE
  ) %>% 
  pivot_wider(id_cols = anio, names_from = flujo, values_from = valor) %>% 
  rename(expo_turistica = `CD_T`, impo_turistica = `DB_T`) %>%  
  mutate(
    balanza = expo_turistica - impo_turistica
  ) %>% 
  select(anio, expo_turistica, impo_turistica, balanza) %>% 
  pivot_longer(
    -anio, names_to = "flujo", values_to = "valor"
  )


