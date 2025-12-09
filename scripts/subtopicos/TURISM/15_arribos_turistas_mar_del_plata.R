# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "arribos_turistas_mar_del_plata.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R484C311' # EMTUR Arribos turistas a Mar del Plata 2007-2024
fuente2 <- 'R483C0' # EMTUR via nota La Capital
fuente3 <- 'R482C0' # Pastoriza

df_emtur <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  arrange(anio)

df_emtur_lc <- argendataR::get_raw_path(fuente2) %>% 
  read.csv() 

df_pastoriza <- argendataR::get_raw_path(fuente3) %>% 
  read.csv()



df_total <- df_emtur_lc %>% 
  rename(arribos_turistas = turistas_cantidad) %>% 
  dplyr::filter(anio< min(df_emtur$anio)) %>% 
  bind_rows(
    df_emtur %>% 
      group_by(anio) %>% 
      summarise(
        arribos_turistas = sum(arribos_turistas, na.rm = T)
      ) %>% 
      ungroup()
  ) %>% 
  mutate(
    fuente = "EMTUR",
    medida = "Arribos turísticos anuales en cantidad de personas",
    tipo_serie = 'original'
  )


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


df_pastoriza_temporada <- df_pastoriza %>% 
  mutate(
    anio_temporada = as.integer(str_extract(temporada, "(\\d{4})-\\d{2}", group = 1)) + 1
  ) %>% 
  select(anio_temporada, arribos_turistas_temporada = totales)


df_temporada <- df_emtur %>% 
  mutate(anio_temporada = dplyr::lead(anio, n=1L, default = 2025)) %>% 
  dplyr::filter(anio_temporada>2007, anio_temporada < year(Sys.Date()), mes %in% c("Diciembre", "Enero", "Febrero")) %>% 
  group_by(anio_temporada) %>% 
  summarise(
    arribos_turistas = sum(arribos_turistas, na.rm = T)
  ) %>% 
  full_join(
    df_emtur_lc %>% 
      dplyr::filter(anio <= 2008) %>% 
      rename(arribos_turistas_totales = turistas_cantidad),
    join_by(anio_temporada == anio)
  ) %>% 
  arrange(anio_temporada) %>% 
  mutate(
    arribos_turistas_temporada_emtur_backward  = impute_backward(arribos_turistas, arribos_turistas_totales),
    arribos_turistas_temporada = ifelse(is.na(arribos_turistas), arribos_turistas_temporada_emtur_backward, arribos_turistas ),
    fuente = "EMTUR",
    tipo_serie = ifelse(is.na(arribos_turistas_temporada), "empalme", "original")
  ) %>% 
  bind_rows(
    df_pastoriza_temporada %>% 
      mutate(fuente = "Pastoriza (2008)",
             tipo_serie = "original")
  ) %>% 
  mutate( medida = "Arribos turísticos en temporada, en cantidad de personas") %>% 
  select(anio = anio_temporada, medida, arribos_turistas = arribos_turistas_temporada, fuente, tipo_serie) %>% 
  arrange(anio)



df_output <- df_temporada %>% 
  arrange(anio) %>%
  bind_rows(
    df_total %>% 
      arrange(anio)
  )
  


