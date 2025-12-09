# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "proporcion_viajera.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R490C0' # YVERA Proporción viajera

df_output <- argendataR::get_raw_path(fuente1) %>% 
  read.csv() %>% 
  mutate(
    proporcion_viajera = 100 * participacion,
    anio = as.integer(indice_tiempo)
  ) %>% 
  select(anio , proporcion_viajera)

