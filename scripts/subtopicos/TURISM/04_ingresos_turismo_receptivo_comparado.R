# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "TURISM"
output_name <- "ingresos_turismo_receptivo_comparado.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R475C0'
fuente2 <- 'R473C0'
fuente3 <- 'R469C309'

df_unwto_manual <- argendataR::get_raw_path(fuente1) %>% 
  read.csv(.)

df_wb <- argendataR::get_raw_path(fuente2) %>% 
  read.csv(.)

df_unwto_inbound_expenditure <- argendataR::get_clean_path(fuente3) %>% 
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
  mutate(geocodigoFundar = ifelse(country_name == "World", "WLD", geocodigoFundar),
         geonombreFundar = ifelse(country_name == "World", "Mundo", geonombreFundar)) %>% 
  select(anio = year, 
         geocodigoFundar, 
         geonombreFundar, 
         int_tourism_receipts_unwto = int_tourism_receipts_usd_billion
         )


df_unwto_inbound_expenditure_complete <- df_unwto_inbound_expenditure %>% 
  mutate(m49_code = as.integer(reporter_area_code)) %>% 
  left_join(geonomenclador,join_by(m49_code)) %>% 
  dplyr::filter(indicator_previous_code == "CP_1_34") %>% 
  mutate(geocodigoFundar = ifelse(reporter_area_code == 5351, "BES", geocodigoFundar),
         geonombreFundar = ifelse(reporter_area_code == 5351, "Bonaire, San Eustaquio y Saba", geonombreFundar)) %>% 
  select(anio = year, geocodigoFundar, geonombreFundar, inbound_expenditure_unwto = value)


df_wb_complete <- df_wb %>% 
  select(anio = year, geocodigoFundar = iso3c,  int_tourism_receipts_wb = `ST.INT.TVLR.CD`) %>% 
  left_join(geonomenclador, join_by(geocodigoFundar)) %>% 
  drop_na(int_tourism_receipts_wb) %>% 
  select(anio, geocodigoFundar, geonombreFundar, int_tourism_receipts_wb) %>% 
  mutate(geonombreFundar = ifelse(geocodigoFundar == "WLD", "Mundo", geonombreFundar))


df_mundo <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar == "WLD") %>% 
  full_join(df_wb_complete %>% 
              dplyr::filter(geocodigoFundar=="WLD"), join_by(anio, geocodigoFundar, geonombreFundar)) %>% 
  arrange(anio) %>% 
  mutate(int_tourism_receipts_unwto_backward = impute_backward(int_tourism_receipts_unwto, int_tourism_receipts_wb),
         int_tourism_receipts_wld = ifelse(is.na(int_tourism_receipts_unwto_backward), int_tourism_receipts_unwto,  int_tourism_receipts_unwto_backward)) %>% 
  select(anio, int_tourism_receipts_wld)


paises_unwto_dashboard <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar !="WLD") %>% 
  distinct(geocodigoFundar) %>% 
  pull()

paises_unwto_inbound <- df_unwto_inbound_expenditure_complete$geocodigoFundar %>% unique()

paises_sin_empalme <- c(
  setdiff(paises_unwto_inbound, paises_unwto_dashboard),
  setdiff(paises_unwto_dashboard, paises_unwto_inbound)
)

paises_empalme <- intersect(paises_unwto_inbound,paises_unwto_dashboard)


df_paises_empalme <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar %in% paises_empalme) %>% 
  full_join(df_unwto_inbound_expenditure_complete %>% 
              dplyr::filter(geocodigoFundar %in% paises_empalme), 
            join_by(anio, geocodigoFundar, geonombreFundar)) %>% 
  arrange(anio) %>% 
  group_by(geocodigoFundar) %>% 
  mutate(int_tourism_receipts_unwto_backward = impute_backward(int_tourism_receipts_unwto, inbound_expenditure_unwto), 
         int_tourism_receipts = ifelse(is.na(int_tourism_receipts_unwto_backward), int_tourism_receipts_unwto,  int_tourism_receipts_unwto_backward)) %>% 
  ungroup() %>% 
  select(anio, geocodigoFundar, geonombreFundar, int_tourism_receipts) %>% 
  arrange(geocodigoFundar, anio)


df_paises_sin_empalme <- df_unwto_complete %>% 
  dplyr::filter(geocodigoFundar %in% paises_sin_empalme) %>% 
  rename(int_tourism_receipts = int_tourism_receipts_unwto) %>% 
  bind_rows(., 
            df_unwto_inbound_expenditure_complete %>% 
              dplyr::filter(geocodigoFundar %in% paises_sin_empalme) %>% 
              rename(int_tourism_receipts = inbound_expenditure_unwto))


df_output <- bind_rows(
  df_paises_empalme %>% mutate(origen = 'empalme'),
  df_paises_sin_empalme %>% mutate(origen = 'original')) %>% 
  left_join(df_mundo, join_by(anio)) %>% 
  mutate(int_tourism_receipts_share = 100 * int_tourism_receipts / int_tourism_receipts_wld) %>% 
  select(anio, geocodigoFundar, geonombreFundar, int_tourism_receipts,int_tourism_receipts_share, origen)


