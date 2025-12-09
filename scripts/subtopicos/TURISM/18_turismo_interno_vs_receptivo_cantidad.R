# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "turismo_interno_vs_receptivo_cantidad.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R489C0' # YVERA Turismo Interno 
fuente2 <- 'R474C0' # YVERA Turismo Receptivo 2010-actualidad

df_yvera_interno <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()

df_yvera_receptivo <- argendataR::get_raw_path(fuente2) %>% 
  read.csv()

df_interno <- df_yvera_interno %>% 
  mutate(anio = year(indice_tiempo),
         tipo_turismo = "Interno") %>% 
  group_by(anio, tipo_turismo) %>% 
  summarise(
    cantidad_turistas = sum(turistas, na.rm = T)
  ) %>% 
  ungroup()

df_receptivo <- df_yvera_receptivo %>% 
  mutate(anio = year(indice_tiempo),
         tipo_turismo = "Receptivo") %>% 
  group_by(anio, tipo_turismo) %>% 
  summarise(
    cantidad_turistas = sum(viajes_de_turistas_no_residentes, na.rm = T)
  ) %>% 
  ungroup()
  

df_output <- bind_rows(
  df_receptivo, df_interno
) %>% 
  dplyr::filter(anio <= (year(Sys.Date())-1))

