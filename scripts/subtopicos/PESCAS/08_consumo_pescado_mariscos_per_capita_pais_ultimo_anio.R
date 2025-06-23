#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "PESCAS"
output_name <- "08_consumo_pescado_mariscos_per_capita_pais_ultimo_anio.csv"
fuente1 <- 'R299C167' # FAO FBS
fuente2 <- 'R300C168' # FAO FBSH

df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_fao_fbsh <- arrow::read_parquet(argendataR::get_clean_path(fuente2))


pescados_mariscos <- c(
  "Fish, Body Oil" = "Pescados y mariscos",
  "Fish, Liver Oi" = "Pescados y mariscos",
  "Freshwater Fish" = "Pescados y mariscos",
  "Demersal Fish" = "Pescados y mariscos",
  "Pelagic Fish" = "Pescados y mariscos",
  "Marine Fish, Other" = "Pescados y mariscos",
  "Crustaceans" = "Pescados y mariscos",
  "Cephalopods" = "Pescados y mariscos", 
  "Molluscs, Other" = "Pescados y mariscos",
  "Aquatic Animals, Others" = "Pescados y mariscos"
  )

df_fao_fbs_filtered <- df_fao_fbs %>% 
  dplyr::filter(item %in% names(pescados_mariscos), element == "Food supply quantity (kg/capita/yr)")  %>% 
  group_by(anio = year, iso3, pais) %>% 
  summarise(value_new = sum(value, na.rm = T)) %>% 
  ungroup()


df_fao_fbsh_filtered <- df_fao_fbsh %>% 
  dplyr::filter(item %in% names(pescados_mariscos), element == "Food supply quantity (kg/capita/yr)")  %>% 
  group_by(anio = year, iso3, pais) %>% 
  summarise(value_old = sum(value, na.rm = T)) %>% 
  ungroup()

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



df_fao_fbs_empalme <- df_fao_fbs_filtered %>% 
  full_join(df_fao_fbsh_filtered, join_by(anio, iso3, pais)) %>% 
  group_by(iso3) %>%
  filter(any(anio == 2010 & !is.na(value_new) & !is.na(value_old))) %>%
  ungroup() %>% 
  arrange(iso3, anio) %>% 
  group_by(iso3) %>% 
  mutate(valor_ = impute_backward(value_new, value_old),
         valor_empalme = ifelse(is.na(value_new), valor_, value_new)) %>% 
  ungroup() 



df_paises <- df_fao_fbs_empalme %>% 
  dplyr::filter(anio == max(anio)) %>% 
  select(iso3, pais, consumo_per_capita = valor_empalme) 


df_mundial <- df_fao_fbs_empalme %>% 
  dplyr::filter(anio == max(anio)) %>% 
  group_by(iso3 = "WLD", pais = "Mundo") %>% 
  summarise(
    consumo_per_capita = mean(valor_empalme, na.rm = T)
  )

df_output <- df_paises %>% 
  bind_rows(df_mundial) %>% 
  arrange(-consumo_per_capita)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  c("iso3"),
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = 'iso3',
    nivel_agregacion = NULL,
  )
