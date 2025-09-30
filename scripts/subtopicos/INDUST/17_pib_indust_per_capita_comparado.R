#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "17_pib_indust_per_capita_comparado"
analista <- "Nicolás Sidicaro"

# rutas 
outstub <- 'indust/input'
auxiliar <- 'indust/auxiliar'

# Diccionario
dicc_pais <- read_csv(file.path(auxiliar,'dicc_paises_UNSD — Methodology.csv'))
dicc_pais <- janitor::clean_names(dicc_pais)
dicc_pais <- dicc_pais %>% select(m49_code,iso_alpha3_code) %>% 
  mutate(m49_code = as.numeric(m49_code)) %>% 
  rename(country_id=m49_code)

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# carga de fuentes - nico 
df_break <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/22',startRow = 3))
df_gdp <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/12',startRow=3))

# ordenar breakdown
df_break <- df_break %>% 
  dplyr::filter(stringr::str_detect(IndicatorName,'ISIC D'))
df_break <- df_break %>% 
  tidyr::pivot_longer(-c(CountryID,Country,IndicatorName),values_to='industry_gdp',names_to='year')
df_break$IndicatorName <- NULL
df_break$year <- as.numeric(df_break$year)

# Ordenar gdp 
df_gdp <- df_gdp %>% 
  tidyr::pivot_longer(-c(CountryID,Country),values_to='gdp_per_capita',names_to='year')
df_gdp$Country <- NULL
df_gdp$year <- as.numeric(df_gdp$year)

# Unir datos 
df_final <- df_break %>% 
  left_join(df_gdp,by=c('year','CountryID'))
df_final <- df_final %>% 
  mutate(industry_gdp = industry_gdp / 100)

# Multiplicar variables 
df_final <- df_final %>% 
  mutate(industry_gdp_per_capita = industry_gdp * gdp_per_capita)

# seleccionar columnar y calcular indice 1970
paises_enteros <- df_final %>% 
  group_by(CountryID) %>% 
  summarize(cantidad_anios = n()) %>% 
  ungroup() %>% 
  filter(cantidad_anios == max(cantidad_anios)) %>% 
  pull(CountryID)

df_final <- df_final %>% 
  select(Country,CountryID,year,industry_gdp_per_capita) %>% 
  filter(CountryID %in% paises_enteros) %>% 
  group_by(Country) %>%
  mutate(
    # Obtener el valor base de 1970 para cada país
    gdp_base_1970 = industry_gdp_per_capita[year == 1970],
    # Calcular el índice (1970 = 100)
    gdp_indust_index = (industry_gdp_per_capita / gdp_base_1970) * 100
  ) %>%
  ungroup() %>% 
  arrange(year)

# Sacar los nulos 
df_final <- df_final %>% 
  filter(!is.na(gdp_indust_index))

# Quedarme con los paises completos 
df_final_aux <- df_final %>% 
  group_by(CountryID) %>% 
  summarize(cantidad = n()) %>% 
  ungroup() %>% 
  filter(cantidad == max(cantidad))

df_final <- df_final %>% 
  filter(CountryID %in% df_final_aux$CountryID)

# Seleccionar variables 
df_final <- df_final %>% 
  select(CountryID,year,gdp_indust_index)
df_final <- df_final %>% 
  left_join(dicc_pais,by=c('CountryID'='country_id'))
df_final <- df_final %>% 
  left_join(dicc_argendata,by=c('iso_alpha3_code'='geocodigo'))
df_final <- df_final %>% 
  select(iso3=iso_alpha3_code,name_long,anio=year,gdp_indust_index)
# guardar resultados 
readr::write_csv(df_final,file.path(outstub,'17_pib_industrial_per_capita_comparado.csv'))
