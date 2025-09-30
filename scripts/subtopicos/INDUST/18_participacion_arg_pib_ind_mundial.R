#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "18_participacion_arg_pib_ind_mundial"
analista <- "Nicolás Sidicaro"

# rutas 
outstub <- 'indust/input'

# carga de fuentes - nico 
df_break <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/6',startRow = 3))
df_mundo <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/7',startRow = 3))

# Diccionario paises 
dicc_pais <- read_csv(file.path('indust/auxiliar/dicc_paises_UNSD — Methodology.csv'))
dicc_pais <- janitor::clean_names(dicc_pais)
dicc_pais <- dicc_pais %>% 
  select(CountryID=m49_code,iso3=iso_alpha3_code)
# Filtrar mundo 
df_mundo <- df_mundo %>% 
  filter(`Region/Subregion` == 'World') %>% 
  filter(IndicatorName == 'Manufacturing (ISIC D)')
df_mundo <- df_mundo %>% 
  tidyr::pivot_longer(-c(`Region/Subregion`,IndicatorName),values_to='industry_gdp',names_to='year')
df_mundo$IndicatorName <- NULL
df_mundo$`Region/Subregion` <- NULL
df_mundo <- df_mundo %>% 
  rename(industry_gdp_wld = industry_gdp)
df_mundo <- df_mundo %>% 
  mutate(year = as.numeric(year)) %>% 
  filter(!is.na(year))
# limpiar nombres 
df_break <- dplyr::rename(df_break,'country'='Country')

# Pivotear 
df_break <- df_break %>% 
  pivot_longer(-c(CountryID,country,IndicatorName),names_to='year',values_to='industry_gdp')
df_break <- df_break %>% 
  filter(IndicatorName == 'Manufacturing (ISIC D)')
df_break$IndicatorName <- NULL
# Agregar ISO 
df_break <- df_break %>% 
  left_join(dicc_pais %>% mutate(CountryID = as.numeric(CountryID)),by=c('CountryID'))
# joinear datos 
df_break <- df_break %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(df_mundo,by=c('year')) 

# filtrar datos sin informacion
df_break <- df_break %>% 
  dplyr::filter(!is.na(industry_gdp))

# calcular peso industrial anual 
df_break <- df_break %>% 
  mutate(prop = industry_gdp / industry_gdp_wld)

# seleccionar var 
df_break <- df_break %>% 
  select(year,iso3,prop_industry_gdp = prop)

# guardar resultados 
readr::write_csv(df_break,file.path(outstub,'18_participacion_arg_pib_ind_mundial.csv'))
