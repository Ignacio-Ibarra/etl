#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "31_consumo_per_capita_pescado_anio_pais.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R299C167' # FAO FBS
fuente2 <- 'R300C168' # FAO FBSH
fuente3 <- 'R398C249' # FAO Annual population
fuente4 <- 'R320C191'  # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS



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
  select(anio, iso3, pais, consumo_per_capita = valor_empalme)  %>% 
  mutate(nivel_agregacion = "pais")


df_population <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.) %>% 
  dplyr::filter(element == "Total Population - Both sexes") %>% 
  select(iso3, anio = year, population = value)


df_mundial_ponderado <- df_paises %>% 
  left_join(df_population, join_by(iso3, anio)) %>% 
  dplyr::filter(!is.na(population), !is.na(consumo_per_capita)) %>% 
  group_by(anio) %>% 
  summarise(
    consumo_per_capita = stats::weighted.mean(consumo_per_capita, population, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(nivel_agregacion = "agregacion",
         iso3 = "WLD", 
         pais = "Mundo") 


df_country_region <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.) %>% 
  dplyr::filter(!is.na(iso3_code)) %>% 
  select(iso3 = iso3_code, region = geo_region_group_es)


df_regiones_ponderado <- df_paises %>% 
  left_join(df_country_region, join_by(iso3)) %>% 
  left_join(df_population, join_by(iso3, anio)) %>% 
  dplyr::filter(!is.na(population), !is.na(consumo_per_capita)) %>% 
  group_by(anio, region) %>% 
  summarise(
    consumo_per_capita = stats::weighted.mean(consumo_per_capita, population, na.rm = T)
  ) %>% 
  ungroup() %>% 
  rename(pais = region) %>% 
  mutate(nivel_agregacion = "agregacion") %>% 
  mutate(
    
    iso3 = case_when(
      pais == "Australia y Nueva Zelandia" ~ "F5501",
      pais == "Polinesia" ~ "F5504",
      pais == "Melanesia" ~ "F5502",
      pais == "Micronesia" ~ "F5503",
      pais == "América central" ~ "F5204",
      pais == "América del Norte" ~ "F5203",
      pais == "América del Sur" ~ "F5207",
      pais == "Asia meridional" ~ "F5303",
      pais == "Asia occidental" ~ "F5305",
      pais == "Asia oriental" ~ "F5302",
      pais == "Asia sudoriental" ~ "F5304",
      pais == "Caribe" ~ "F5206",
      pais == "Europa del Norte" ~ "F5402",
      pais == "Europa meridional" ~ "F5403",
      pais == "Europa occidental" ~ "F5404",
      pais == "Europa oriental" ~ "F5401",
      pais == "África central" ~ "F5102",
      pais == "África occidental" ~ "F5105",
      pais == "África oriental" ~ "F5101",
      pais == "África septentrional" ~ "F5103",
      pais == "África austral" ~ "F5104",
      pais == "Asia central" ~ "F5301"
    )
    
  )
  


df_output <- bind_rows(df_paises,df_regiones_ponderado, df_mundial_ponderado)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c('anio','iso3')

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
    columna_geo_referencia = 'iso3',
    nivel_agregacion = NULL
  )