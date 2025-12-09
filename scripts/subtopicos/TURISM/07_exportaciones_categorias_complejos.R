# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "exportaciones_categorias_complejos.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R478C310'
fuente2 <- 'R323C196'


df_cei <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_indec <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()


df_expo_bienes <- df_indec %>% 
  mutate(tipo = "Bienes") %>% 
  dplyr::filter(grepl("^Complej|Resto de exportaciones", complejos)) %>% 
  rename(categoria = complejos, 
         exportaciones_en_usd_mill = valores) 
  


df_expo_servicios <- df_cei %>% 
  dplyr::filter(categoria =="Total de SBC"|agrupamiento != "BASADOS EN EL CONOCIMIENTO",
                categoria != "Total") %>% 
  group_by(anio, categoria) %>% 
  summarise(
    exportaciones_en_usd_mill = sum(exportaciones_en_usd_mill)
  ) %>% 
  ungroup() %>% 
  mutate(
    categoria = ifelse(categoria == "Total de SBC", "Servicios basados en conocimiento", categoria)
  ) %>% 
  mutate(tipo = "Servicios")



df_output <- bind_rows(
  
  df_expo_bienes %>% 
    dplyr::filter(anio %in% min(df_expo_servicios$anio):max(df_expo_servicios$anio)),
  
  df_expo_servicios %>% 
    dplyr::filter(anio %in% min(df_expo_bienes$anio):max(df_expo_bienes$anio))
  
  
)
  
 