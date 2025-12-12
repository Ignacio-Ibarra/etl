# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "arribos_turisticos_comparado.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"
fuente1 <- 'R476C0' # UNWTO Manual Dashboard
fuente2 <- 'R469C306' # UNWTO inbound arraivals
fuente3 <- 'R491C315' # OWID - International trips by region

# Fuentes de Argentina
fuente4 <- 'R488C0' # YVERA Turismo Receptivo 1990-2010
fuente5 <- 'R474C0' # YVERA Turismo Receptivo 2010-actualidad
fuente6 <- 'R485C314' # Fundacion Norte y Sur - Producción (Serie 2) - LLegadas Turistas 


#############################
# CARGO DATASETS

df_unwto_manual <- argendataR::get_raw_path(fuente1) %>% 
  read.csv(.)

df_unwto_inbound_arraivals <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_owid <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_yvera_1990_2010 <- argendataR::get_raw_path(fuente4) %>% 
  readxl::read_excel()

df_yvera_2010_actualidad <- argendataR::get_raw_path(fuente5) %>% 
  read.csv()

df_fnys <- argendataR::get_clean_path(fuente6) %>% 
  arrow::read_parquet()


geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  dplyr::filter(!is.na(m49_code_unsd)) %>% 
  select(m49_code = m49_code_unsd, 
         geocodigoFundar = codigo_fundar, 
         geonombreFundar = desc_fundar)


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


df_unwto_complete <- df_unwto_manual %>% 
  mutate(m49_code = as.integer(m49_code)) %>% 
  left_join(geonomenclador,join_by(m49_code)) %>% 
  mutate(geocodigoFundar = ifelse(country == "World", "WLD", geocodigoFundar),
         geonombreFundar = ifelse(country == "World", "Mundo", geonombreFundar)) %>% 
  select(anio = year, 
         geocodigoFundar, 
         geonombreFundar, 
         int_tourism_arraivals_unwto = int_tourist_arraivals_million
  ) %>% 
  drop_na(geocodigoFundar)


df_unwto_inbound_arraivals_complete <- df_unwto_inbound_arraivals %>% 
  mutate(m49_code = as.integer(reporter_area_code)) %>% 
  left_join(geonomenclador,join_by(m49_code)) %>% 
  dplyr::filter(indicator_previous_code == "CP_1_2") %>% 
  mutate(geocodigoFundar = ifelse(reporter_area_code == 5351, "BES", geocodigoFundar),
         geonombreFundar = ifelse(reporter_area_code == 5351, "Bonaire, San Eustaquio y Saba", geonombreFundar),
         inbound_arraivals_unwto = value / 1000) %>% 
  select(anio = year, geocodigoFundar, geonombreFundar, inbound_arraivals_unwto)


######################################
# SERIE ARGENTINA

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


df_serie_argentina <- df_yvera_turismo_receptivo %>% 
  full_join(df_fnys_arraivals, join_by(anio)) %>% 
  arrange(anio) %>% 
  fill(geocodigoFundar:geonombreFundar, .direction = "updown") %>% 
  mutate(
    arribos_turistas = ifelse(is.na(arribos_turistas_yvera), impute_backward(arribos_turistas_yvera, arribos_turistas_fnys), arribos_turistas_yvera)
  ) %>% 
  select(anio, geocodigoFundar, geonombreFundar, arribos_turistas) 


##############################
# SERIES PAISES


paises_unwto_dashboard <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar !="WLD") %>% 
  group_by(geocodigoFundar) %>% 
  ungroup() %>% 
  distinct(anio, geocodigoFundar)



paises_unwto_inbound_arraivals <- df_unwto_inbound_arraivals_complete %>% 
  distinct(anio, geocodigoFundar)


paises_empalme <- paises_unwto_dashboard %>% 
  inner_join(paises_unwto_inbound_arraivals, join_by(anio, geocodigoFundar)) %>% 
  group_by(geocodigoFundar) %>% 
  dplyr::filter(anio == min(anio)) %>% 
  pull(geocodigoFundar)


paises_sin_empalme <- unique(c(
  setdiff(paises_unwto_dashboard$geocodigoFundar %>% unique(), paises_empalme),
  setdiff(df_unwto_inbound_arraivals_complete$geocodigoFundar %>% unique(), paises_empalme)
))



df_paises_empalme <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar %in% paises_empalme) %>% 
  full_join(df_unwto_inbound_arraivals_complete %>% 
              dplyr::filter(geocodigoFundar %in% paises_empalme), 
            join_by(anio, geocodigoFundar, geonombreFundar)) %>% 
  arrange(anio) %>% 
  group_by(geocodigoFundar) %>% 
  mutate(int_tourism_arraivals_unwto_backward = impute_backward(int_tourism_arraivals_unwto, inbound_arraivals_unwto), 
         int_tourism_arraivals = ifelse(is.na(int_tourism_arraivals_unwto_backward), int_tourism_arraivals_unwto,  int_tourism_arraivals_unwto_backward)) %>% 
  ungroup() %>% 
  select(anio, geocodigoFundar, geonombreFundar, int_tourism_arraivals) %>% 
  arrange(geocodigoFundar, anio)


df_paises_sin_empalme <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar %in% paises_sin_empalme) %>% 
  rename(int_tourism_arraivals = int_tourism_arraivals_unwto) %>% 
  bind_rows(., 
            df_unwto_inbound_arraivals_complete %>% 
              dplyr::filter(geocodigoFundar %in% paises_sin_empalme) %>% 
              rename(int_tourism_arraivals = inbound_arraivals_unwto))


df_serie_paises <- bind_rows(
  df_paises_empalme ,
  df_paises_sin_empalme) 



###############################################
# SERIE MUNDIAL (modificada por Argentina)


geo_codes <- c(
  "Europe" = "ZEUR",
  "Americas" = "X21",
  "Asia and the Pacific" = "APA",
  "Asia & Pacific" = "APA",
  "Middle East" = "DESHUM_AHDI.MEA",
  "Africa" = "X06"
)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_unwto_complete_regiones <- df_unwto_manual %>% 
  dplyr::filter(is.na(m49_code), country != "World") %>% 
  mutate(geocodigoFundar = geo_codes[country]) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio = year, geocodigoFundar, geonombreFundar, arribos_turisticos_internacionales_millon = int_tourist_arraivals_million)

df_owid_complete <- df_owid %>% 
  mutate(geocodigoFundar = geo_codes[regiones]) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  mutate(arribos_turisticos_internacionales_millon = arribos_turisticos_internacionales/1000000) %>% 
  select(anio, geocodigoFundar, geonombreFundar, arribos_turisticos_internacionales_millon)


df_serie_argentina_unwto_empalme <- df_serie_argentina %>% 
  full_join(df_serie_paises %>% 
              dplyr::filter(geocodigoFundar == "ARG"), join_by(anio, geocodigoFundar, geonombreFundar)) %>% 
  mutate(int_tourism_arraivals_corregido = 1000000 * case_when(
    is.na(int_tourism_arraivals) ~ impute_backward(int_tourism_arraivals, arribos_turistas),
    TRUE ~ int_tourism_arraivals
  )) %>%
  select(anio, int_tourism_arraivals_unwto_empalme_arg = int_tourism_arraivals_corregido)


# sumo las regiones
df_mundo <- bind_rows(
  df_unwto_complete_regiones,
  df_owid_complete %>% 
    dplyr::filter(anio<2016)
  ) %>% 
  group_by(anio) %>% 
  summarise(
    arribos_turisticos_internacionales_mundo = 1000000 * sum(arribos_turisticos_internacionales_millon, na.rm = T)
  )  %>% 
  left_join(df_serie_argentina_unwto_empalme, join_by(anio)) %>% 
  left_join(df_serie_argentina %>% 
              select(anio, arribos_turistas), join_by(anio)) %>% 
  drop_na(arribos_turistas) %>% 
  mutate(
    int_tourism_arraivals_wld_corregido = (arribos_turisticos_internacionales_mundo + arribos_turistas - int_tourism_arraivals_unwto_empalme_arg)/1000000
  ) %>% 
  select(anio, int_tourism_arraivals_wld_corregido)
  
################################
# Armado serie comparada


df_output <- bind_rows(
  df_serie_paises %>% 
    dplyr::filter(geocodigoFundar != "ARG"),
  df_serie_argentina %>% 
    mutate(int_tourism_arraivals = arribos_turistas/1000000)
) %>% 
  inner_join(df_mundo, join_by(anio)) %>% 
  mutate(
    share_int_tourism_arraivals = 100 * int_tourism_arraivals / int_tourism_arraivals_wld_corregido
  ) %>% 
  select(anio, geocodigoFundar, geonombreFundar, int_tourism_arraivals, share_int_tourism_arraivals)



