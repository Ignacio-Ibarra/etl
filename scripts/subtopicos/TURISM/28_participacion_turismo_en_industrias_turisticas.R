# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "participacion_turismo_en_industrias_turisticas.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R493C316'

df_cst_yvera <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


filtro <- c("A.1. Productos característicos del turismo" = "Media industrias turísticas", 
            "1. Servicios de alojamiento para visitantes" = "Alojamiento",
            "2. Servicios de provisión de alimentos y bebidas" = "Gastronomía",
            "3.a. Servicios de transporte aéreo de pasajeros" = "Transporte aéreo de pasajeros",
            "4. Agencias de viajes y otros servicios de reserva" = "Agencias de viaje",
            "3.b. Resto servicios de transporte de pasajeros" = "Otros transportes", 
            "5. Otros servicios" = "Otros servicios", 
            "A.2. Otros productos no característicos" = "Otros productos no característicos"
            )

df_output <- df_cst_yvera %>% 
  dplyr::filter(grepl("Ratios", categoria),
                productos %in% names(filtro)) %>% 
  mutate(industria_turistica = filtro[productos]) %>% 
  select(industria_turistica, ratio = valor)



