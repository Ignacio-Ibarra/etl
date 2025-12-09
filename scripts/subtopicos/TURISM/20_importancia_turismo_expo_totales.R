# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "importancia_turismo_expo_totales.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R479C0'


df_imf <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()


df_output <- df_imf %>% 
  dplyr::filter(COUNTRY == "ARG", 
                BOP_ACCOUNTING_ENTRY == "CD_T", 
                INDICATOR %in% c("SD", "GS")) %>% 
  select(
    anio = TIME_PERIOD,
    indicador = INDICATOR,
    valor = OBS_VALUE
         ) %>% 
  pivot_wider(id_cols = anio, names_from = indicador, values_from = valor) %>% 
  rename(expo_turistica = `SD`) %>%  
  mutate(
    prop_expo = 100 * expo_turistica / `GS`
  ) %>% 
  select(anio, expo_turistica, prop_expo)


