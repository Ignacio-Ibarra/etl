#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "01_pib_indust_per_capita"
analista <- "Nicolás Sidicaro"
fuente1 <- 'GDP Per capita' # UN - National Accounts - Analysis of Main Aggregates (AMA)
fuente2 <- 'GDP Breakdown (%)' # UN - National Accounts - Analysis of Main Aggregates (AMA)

# Funciones
source('indust/utils/rellenar_valores_faltantes_forward_looking.R')

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
# carga de fuentes - Argendata 
# df_gdp <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()
# df_pop <- argendataR::get_clean_path(fuente2) %>% 
#   arrow::read_parquet()

# carga de fuentes - nico 
df_break <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/22',startRow = 3))
df_gdp <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/9',startRow = 3))

# ordenar breakdown
df_break <- df_break %>% 
  dplyr::filter(stringr::str_detect(IndicatorName,'ISIC D|ISIC C-E'))
df_break <- df_break %>% 
  tidyr::pivot_longer(-c(CountryID,Country,IndicatorName),values_to='industry_gdp',names_to='year')
df_break <- df_break %>% 
  tidyr::pivot_wider(names_from=IndicatorName,values_from = industry_gdp)

df_break$year <- as.numeric(df_break$year)
# Calcular variacion interanual de C-E
df_break <- janitor::clean_names(df_break)
df_break <- df_break %>% 
  group_by(country) %>% 
  mutate(interanual_var = (lead(mining_manufacturing_utilities_isic_c_e) - mining_manufacturing_utilities_isic_c_e) / mining_manufacturing_utilities_isic_c_e)

# Aplicar la función para rellenar casos faltantes
df_break <- df_break %>% 
  fill_missing_backwards("manufacturing_isic_d", "interanual_var",
                         new_col_name = "manufacturing_isic_d_complete" ,
                         group_vars = c("country_id", "country"))

# Reemplazar casos faltantes
df_break <- df_break %>% 
  mutate(manufacturing_isic_d = if_else(is.na(manufacturing_isic_d) & !is.na(interanual_var),
                                        manufacturing_isic_d_complete,
                                        manufacturing_isic_d))
df_break$interanual_var <- NULL
df_break$manufacturing_isic_d_complete <- NULL
df_break$mining_manufacturing_utilities_isic_c_e <- NULL

# pivotear df_gdp 
df_gdp <- df_gdp %>% 
  pivot_longer(-c(CountryID,Country),values_to='gdp_per_cap',names_to='year')
df_gdp$year <- as.numeric(df_gdp$year)
# limpiar nombres 
df_gdp <- janitor::clean_names(df_gdp)
df_break <- janitor::clean_names(df_break)

# joinear datos 
df_join <- df_gdp %>% 
  dplyr::left_join(df_break,by=c('country_id','country','year'))

# filtrar datos sin informacion
df_join <- df_join %>% 
  dplyr::filter(!is.na(manufacturing_isic_d))

df_join <- df_join %>% 
  dplyr::mutate(gdp_indust_pc = (gdp_per_cap*(manufacturing_isic_d/100)))

# sacar los que no tienen datos 
df_join <- df_join %>% 
  dplyr::filter(!is.na(gdp_per_cap))

# seleccionar columnas
df_join <- df_join %>% 
  select(country_id,country,year,gdp_indust_pc)

# Nombre de pais 
df_join <- df_join %>% 
  left_join(dicc_pais,by='country_id')
df_join <- df_join %>% 
  left_join(dicc_argendata,by=c('iso_alpha3_code'='geocodigo'))
df_join <- df_join %>% 
  select(iso_alpha3_code,name_long,year,gdp_indust_pc)

# guardar resultados 
readr::write_csv(df_join,file.path(outstub,'01_pib_industrial_per_capita.csv'))
