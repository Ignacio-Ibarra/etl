# Metadatos 
subtopico <- "INDUST"
output_name <- "15_impo_gdp_variacion"
analista <- "Nicolás Sidicaro"
fuente1 <- '' #

# rutas 
instub <- 'indust/auxiliar/expo_impo_serie_larga/'
outstub <- 'indust/input'

# Cargar datos 
df_comex <- read_csv(file.path(instub,'importaciones_por_pais_1962_2023.csv'))
df_gdp <- readr::read_csv('http://149.50.137.164:2147/static/etl-fuentes/raw/WDI_gdp_constant_us.csv')

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# Filtrar productos no clasificados 
df_comex <- df_comex %>% 
  filter(! lall_desc_full %in% c('Otros','Transacciones no clasificadas'))

# Agrupar 
df_comex <- df_comex %>% 
  mutate(manufacturas = if_else(str_detect(lall_desc_full,'Manufacturas'),'Industrial','No industrial'))


# calcular importaciones 
df_comex <- df_comex %>% 
  group_by(year,manufacturas,location_code) %>% 
  summarize(importaciones = sum(impo))

# calcular variacion de las importaciones industriales 
df_comex <- df_comex %>% 
  filter(manufacturas == 'Industrial')
df_comex$manufacturas <- NULL
df_comex <- df_comex %>% 
  filter(importaciones > 0)
df_comex <- df_comex %>%
  arrange(year) %>% 
  group_by(location_code) %>% 
  mutate(variacion_interanual = (importaciones/lag(importaciones)) - 1 )

# Seleccionar variables y filtrar datos
df_comex$importaciones <- NULL
df_comex <- df_comex %>% 
  filter(!is.na(variacion_interanual))
df_comex <- df_comex %>% 
  rename(importaciones_industriales=variacion_interanual)

# Armar variacion gdp 
df_gdp <- df_gdp %>% 
  select(year,iso3c,gdp=NY.GDP.MKTP.KD)
df_gdp <- df_gdp %>% 
  arrange(year) %>% 
  group_by(iso3c) %>% 
  mutate(variacion_interanual = (gdp/lag(gdp)) - 1 )
df_gdp <- df_gdp %>% 
  filter(!is.na(gdp)) %>% 
  filter(!is.na(variacion_interanual))
df_gdp <- df_gdp %>% 
  filter(!is.na(iso3c))
df_gdp$gdp <- NULL
df_gdp <- df_gdp %>% 
  rename(gdp=variacion_interanual)

# juntar bases 
df_final <- df_comex %>% 
  left_join(df_gdp,by=c('year'='year','location_code'='iso3c'))

# Pivotear
df_final <- df_final %>% 
  pivot_longer(-c(year,location_code),names_to='dato',values_to='variacion_interanual')

# Nombres de variables 
df_final <- df_final %>% 
  rename(iso3=location_code,variable = dato) %>% 
  mutate(variable = if_else(variable == 'gdp','Var. interanual del PIB','Var. interanual de las importaciones industriales'))

# guardar resultados 
readr::write_csv(df_final,file.path(outstub,paste0(output_name,'.csv')))