# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "pasajeros_avion_tipo_vuelo.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R485C312' # Fundación Norte y Sur - Producción Serie 2 - Aviación
fuente2 <- 'R486C313' # ANAC Serie Historica 2001 - 2022
fuente3 <- 'R487C0' # SINTA - Vuelos, asientos y pasajeros por día. Vuelos, asientos y pasajeros por día según clasificación de vuelo


df_fnys <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_anac <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() 

df_sinta <- argendataR::get_raw_path(fuente3) %>% 
  read.csv()


df_pasajeros_fnys <- df_fnys %>% 
  dplyr::filter(indicador == "Pasajeros transportados") %>% 
  select(anio, tipo_vuelo, pasajeros_miles_fnys = valor)


df_pasajeros_anac_stage <- df_anac %>% 
  dplyr::filter(nro_tabla %in% c("TABLA 7", "TABLA 8"), categoria == "Comercial") %>%
  group_by(anio, nombre_variable) %>% 
  summarise(
    pasajeros_miles = sum(valor, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(
    tipo_vuelo = ifelse(grepl("Cabotaje", nombre_variable), "Cabotaje", "Internacional")
  ) %>% 
  select(anio, tipo_vuelo, pasajeros_miles)
  

df_pasajeros_anac_total <- df_pasajeros_anac_stage %>% 
  mutate(
    tipo_vuelo = "Total"
  ) %>% 
  group_by(anio, tipo_vuelo) %>% 
  summarise(
    pasajeros_miles = sum(pasajeros_miles, na.rm = T)
  ) %>% 
  ungroup()

df_pasajeros_anac <- bind_rows(df_pasajeros_anac_stage, df_pasajeros_anac_total) %>% 
  rename(pasajeros_miles_anac = pasajeros_miles)



df_pasajeros_sinta_stage <- df_sinta %>% 
  mutate(
    anio = year(indice_tiempo)
  ) %>% 
  dplyr::filter(anio < year(Sys.Date())) %>% 
  group_by(anio, clasificacion_vuelo) %>% 
  summarise(
    pasajeros_miles_sinta = sum(pasajeros, na.rm = T) / 1000
  )

df_pasajeros_sinta_total <- df_pasajeros_sinta_stage %>% 
  mutate(clasificacion_vuelo = "Total") %>% 
  group_by(anio, clasificacion_vuelo) %>% 
  summarise(
    pasajeros_miles_sinta = sum(pasajeros_miles_sinta)
  )


df_pasajeros_sinta <- bind_rows(df_pasajeros_sinta_stage, df_pasajeros_sinta_total)


impute_backward <- function(A, B) {
  # Calcular las variaciones relativas de B
  result <- rep(NA_real_, length(A))
  
  VarB <- B / dplyr::lag(B)
  
  # Encontrar el primer índice con un valor no nulo en A
  t0 <- min(which(!is.na(A)))
  
  result[t0] = A[t0]
  
  # Imputar hacia atrás
  for (t in (t0 - 1):1) {
    if (!is.na(VarB[t + 1]) & is.na(A[t])) {
      result[t] <- result[t + 1] / VarB[t + 1]
    }
  }
  
  return(result)
}



df_output <- df_pasajeros_fnys %>% 
  full_join(df_pasajeros_anac, join_by(anio, tipo_vuelo)) %>% 
  full_join(df_pasajeros_sinta, join_by(anio, tipo_vuelo == clasificacion_vuelo)) %>% 
  arrange(anio) %>% 
  group_by(tipo_vuelo) %>% 
  mutate(
    pasajeros_miles_sinta_anac = impute_backward(pasajeros_miles_sinta, pasajeros_miles_anac), 
    pasajeros_miles_sinta_anac_fnys = impute_backward(pasajeros_miles_sinta_anac, pasajeros_miles_fnys),
    pasajeros_miles = case_when(
      !is.na(pasajeros_miles_sinta) ~ pasajeros_miles_sinta, 
      !is.na(pasajeros_miles_sinta_anac) ~ pasajeros_miles_sinta_anac,
      !is.na(pasajeros_miles_sinta_anac_fnys) ~ pasajeros_miles_sinta_anac_fnys
      
    ),
    fuente = case_when(
      !is.na(pasajeros_miles_sinta) ~ "SINTA", 
      !is.na(pasajeros_miles_sinta_anac) ~ "ANAC",
      !is.na(pasajeros_miles_sinta_anac_fnys) ~ "Fundación Norte y Sur"
    )
  ) %>% 
  select(anio, tipo_vuelo, fuente, pasajeros_miles) %>% 
  group_by(anio) %>% 
  fill(fuente, .direction = "downup") %>% 
  ungroup()



  
