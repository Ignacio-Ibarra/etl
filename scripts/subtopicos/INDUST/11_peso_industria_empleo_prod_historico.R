#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "11_peso_industria_empleo_historico"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R35C106' # Puestos CGI 
fuente2 <- 'GDP Breakdown (%)' # UN - National Accounts - Analysis of Main Aggregates (AMA)
fuente3 <- 'R231C102' # Groningen Growth and Development Centre

# Funciones
source('indust/utils/empalme_serie_empleo_historica.R')


# rutas 
outstub <- 'indust/input'

# Diccionario paises
dicc_pais <- read_csv(file.path(outstub,'01_pib_industrial_per_capita.csv')) %>% 
  select(country,iso3c) %>% 
  distinct()
paises_agregados <- data.frame(country=c('Bolivia (Plurinational State of)','Egypt','U.R. of Tanzania: Mainland',
                                         'Republic of Korea','Taiwan','China, Hong Kong SAR',
                                         "Lao People's DR",'Türkiye'),
                               iso3c = c('BOL','EGY','TZA','KOR','TWN','HKG',
                                         'LAO','TUR'))
dicc_pais <- bind_rows(dicc_pais,paises_agregados)

# carga de fuentes - Argendata 
# df_cgi <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()
# df_break <- argendataR::get_clean_path(fuente2) %>% 
#   arrow::read_parquet()
# df_empleo <- argendataR::get_clean_path(fuente3) %>% 
#   arrow::read_parquet()


# carga de fuentes - nico 
df_break <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/22',startRow = 3))
df_empleo <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/ETD_10SD_EASD_CLEAN.parquet')
df_cgi <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/puestos_serie_cgi_CLEAN.parquet')
df_break_arg <- read_csv('https://raw.githubusercontent.com/argendatafundar/data/main/ACECON/7_pib_comp_va.csv')

# ordenar breakdown
df_break <- df_break %>% 
  dplyr::filter(stringr::str_detect(IndicatorName,'ISIC D'))
df_break <- df_break %>% 
  tidyr::pivot_longer(-c(CountryID,Country,IndicatorName),values_to='industry_gdp',names_to='year')
df_break$IndicatorName <- NULL
df_break$CountryID <- NULL
df_break$year <- as.numeric(df_break$year)

# filtrar datos de empleo
df_empleo <- df_empleo %>% 
  filter(var_code == 'EMP' & !is.na(value))

# calcular participacion industria empleo 
df_empleo <- df_empleo %>% 
  group_by(year,cnt,sector) %>% 
  summarize(value = sum(value))
df_empleo <- df_empleo %>% 
  group_by(year,cnt) %>% 
  mutate(prop = value / sum(value))

# Agregar nombre de pais 
df_empleo <- df_empleo %>% 
  left_join(dicc_pais,by=c('cnt'='iso3c'))

# Agregar empleo CGI 
df_cgi <- df_cgi %>% 
  filter(trim == 'Total')
df_cgi <- df_cgi %>% 
  filter(indicador %in% c('Total general','Industria manufacturera'))
df_cgi <- df_cgi %>%  
  select(-c(letra,trim)) %>% 
  pivot_wider(names_from = indicador,values_from=puestos)
df_cgi <- df_cgi %>% 
  mutate(prop = `Industria manufacturera` / `Total general`)
df_cgi <- df_cgi %>% 
  select(year=anio,prop) %>% 
  mutate(cnt = 'ARG',
         sector = 'Manufacturing',
         value = NA_real_,
         country = 'Argentina')

# Tirar serie de argentina (CGI) para atras con GGDC
df_empleo_arg <- df_empleo %>% 
  filter(country == 'Argentina')
df_empleo_arg <- df_empleo_arg %>% 
  filter(sector == 'Manufacturing')

# Ejecutar el empalme
resultado <- empalmar_series(
  df_historico = df_empleo_arg,
  df_oficial = df_cgi,
  var_empalme = "prop",
  grupos = c("cnt", "sector", "country"),
  año_empalme = 2016
)

# Obtener la serie final
serie_final <- resultado$serie_empalmada
serie_final$fuente <- NULL

# Juntar la serie 
df_empleo$value <- NULL
df_empleo <- df_empleo %>% 
  filter(cnt != 'ARG')
serie_final$value <- NULL
df_empleo <- bind_rows(df_empleo,serie_final)

# Armar dato de share de PIB Industrial en Argentina 
df_break <- df_break %>% 
  filter(Country != 'Argentina')
df_break_arg <- df_break_arg %>% 
  filter(sector == 'industria_manufacturera')
df_break_arg <- df_break_arg %>% 
  mutate(Country = 'Argentina') %>% 
  select(-sector)
df_break_arg <- df_break_arg %>% 
  select(Country,year=anio,industry_gdp = valor)
df_break <- bind_rows(df_break,df_break_arg)

# Agregar tasas de gdp industry
df_final <- df_empleo %>% 
  filter(sector == 'Manufacturing') %>% 
  select(-sector) %>% 
  full_join(df_break,by=c('country'='Country','year'='year')) %>% 
  mutate(cnt = if_else(country == 'Argentina','ARG',cnt))

# Filtrar manufacturas
df_final <- df_final %>% 
  rename(share_industrial_employment=prop,
         share_industrial_gdp = industry_gdp) %>% 
  mutate(share_industrial_gdp = share_industrial_gdp / 100)

# Pivotear 
df_final <- df_final %>% 
  pivot_longer(-c(year,cnt,country),names_to='variable',values_to='valor')

# Sacar nulos 
df_final <- df_final %>% 
  filter(!is.na(valor))

# guardar resultados 
readr::write_csv(df_final,file.path(outstub,'11_peso_industria_empleo_prod_historico.csv'))
