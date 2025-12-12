# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "turismo_receptivo_largo_plazo.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R488C0' # YVERA Turismo Receptivo 1990-2010
fuente2 <- 'R474C0' # YVERA Turismo Receptivo 2010-actualidad
fuente3 <- 'R485C314' # Fundacion Norte y Sur - Producción (Serie 2) - LLegadas Turistas 


df_yvera_1990_2010 <- argendataR::get_raw_path(fuente1) %>% 
  readxl::read_excel()

df_yvera_2010_actualidad <- argendataR::get_raw_path(fuente2) %>% 
  read.csv()

df_fnys <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()


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


df_yvera_turismo_receptivo_old <- df_yvera_1990_2010 %>% 
  group_by(anio = `Año`) %>% 
  summarise(
    arribos_turistas_yvera = sum(Viajes, na.rm = T) 
  ) %>% 
  ungroup() 


df_yvera_turismo_recpetivo_new <- df_yvera_2010_actualidad %>% 
  mutate(anio = year(indice_tiempo)) %>% 
  group_by(anio) %>% 
  summarise(
    arribos_turistas_yvera = sum(viajes_de_turistas_no_residentes, na.rm = T) 
  ) %>% 
  ungroup() 

df_yvera_turismo_receptivo <- bind_rows(
  df_yvera_turismo_receptivo_old %>% 
    dplyr::filter(anio<2010), 
  df_yvera_turismo_recpetivo_new
) %>% 
  mutate(geocodigoFundar = "ARG", geonombreFundar = "Argentina") %>% 
  select(anio, geocodigoFundar, geonombreFundar, arribos_turistas_yvera)
  


df_fnys_arraivals <- df_fnys %>% 
  dplyr::filter(detalle == "Total") %>% 
  dplyr::filter(anio<= min(df_yvera_turismo_receptivo$anio)) %>% 
  select(anio, arribos_turistas_fnys = valor)


df_output <- df_yvera_turismo_receptivo %>% 
  full_join(df_fnys_arraivals, join_by(anio)) %>% 
  arrange(anio) %>% 
  fill(geocodigoFundar:geonombreFundar, .direction = "updown") %>% 
  mutate(
    arribos_turistas = ifelse(is.na(arribos_turistas_yvera), impute_backward(arribos_turistas_yvera, arribos_turistas_fnys), arribos_turistas_yvera)
  ) %>% 
  select(anio, geocodigoFundar, geonombreFundar, arribos_turistas)


