# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "dist_turismo_receptivo_por_origen_turista.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R95C187'


df_indec_cabps <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


limitrofes <- c("BRA", "CHL", "URY", "BOL", "PRY")

df_output <- df_indec_cabps %>% 
  dplyr::filter(anio %in% 2016:2024,
                operacion == "Crédito",
                descripcion_cabps2010  == "Viajes",
                !(descripcion_pais %in% c("Mundo", "Unión Europea"))) %>% 
  group_by(iso2, iso3, descripcion_pais) %>% 
  summarise(
    gasto_receptivo = sum(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    categoria = ifelse(iso3 %in% limitrofes, "Limítrofe", "No limítrofe")
  ) %>% 
  mutate(
    distribucion_gasto_receptivo = 100 * gasto_receptivo / sum(gasto_receptivo)
  )


