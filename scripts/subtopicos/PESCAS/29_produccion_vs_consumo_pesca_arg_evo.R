#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "29_produccion_vs_consumo_pesca_arg_evo.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R321C193' # FAO Fisheries and Aquaculture Data Collection. Global Capture Production - File: Capture_Quantity.csv
fuente3 <- 'R398C249' # FAO Anual Population
fuente4 <- 'R299C167' # FAO FBS
fuente5 <- 'R300C168' # FAO FBSH



# PRODUCCION
df_captura <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_acuicola <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_prod_acuicola_arg <- df_acuicola %>% 
  dplyr::filter(country_un_code == 32) %>% 
  group_by(anio = period, country_un_code) %>% 
  summarise(produccion_acuicola = sum(value, na.rm = T)) %>% 
  ungroup()


df_prod_pesquera_arg <- df_captura %>% 
  dplyr::filter(country_un_code==32) %>% 
  group_by(anio = period, country_un_code) %>% 
  summarise(produccion_captura = sum(value, na.rm = T)) %>% 
  ungroup()


df_produccion_total_arg <- df_prod_acuicola_arg %>% 
  full_join(df_prod_pesquera_arg, join_by(anio, country_un_code)) %>% 
  dplyr::filter(!(is.na(produccion_acuicola) & is.na(produccion_captura))) %>% 
  mutate(produccion_total = produccion_acuicola + produccion_captura) %>% 
  select(anio, produccion_total)

# POBLACION 

df_poblacion_arg <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.) %>% 
  dplyr::filter(iso3 == "ARG", element == "Total Population - Both sexes") %>% 
  mutate(poblacion = value * 1000) %>% 
  select(anio = year, poblacion)


# PRODUCCION PER CAPITA

df_prod_pc <- df_produccion_total_arg %>% 
  left_join(df_poblacion_arg, join_by(anio) ) %>% 
  mutate(prod_kg_pc = produccion_total * 1000 / poblacion) %>% 
  select(anio, prod_kg_pc)

# CONSUMO PER CAPITA

df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente4)) 

df_fao_fbsh <- arrow::read_parquet(argendataR::get_clean_path(fuente5))


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



df_cons_pc <- df_fao_fbs_filtered %>% 
  full_join(df_fao_fbsh_filtered, join_by(anio, iso3, pais)) %>% 
  group_by(iso3) %>%
  filter(any(anio == 2010 & !is.na(value_new) & !is.na(value_old))) %>%
  ungroup() %>% 
  arrange(iso3, anio) %>% 
  group_by(iso3) %>% 
  mutate(valor_ = impute_backward(value_new, value_old),
         valor_empalme = ifelse(is.na(value_new), valor_, value_new)) %>% 
  ungroup() %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  rename(consumo_kg_pc = valor_empalme) %>% 
  select(anio, consumo_kg_pc)


df_output <- df_prod_pc %>% 
  inner_join(df_cons_pc, join_by(anio))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c('anio')

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = pk, # variables pk del dataset para hacer el join entre bases
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
    pk =  pk,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )