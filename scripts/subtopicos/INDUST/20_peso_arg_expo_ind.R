#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
options(scipen=999) # notacion cientifica
# Metadatos 
subtopico <- "INDUST"
output_name <- "20_peso_arg_expo_ind"
analista <- "Nicolás Sidicaro"
fuente1 <- 'sitc_country_product_year_2.csv' # Harvard Complexity Atlas   

# rutas 
instub <- 'indust/auxiliar/expo_impo_serie_larga/'
outstub <- 'indust/input'

# Cargar datos 
df_comex <- read_csv(file.path(instub,'exportaciones_por_pais_1962_2023.csv'))

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# Filtrar productos no clasificados 
df_comex <- df_comex %>% 
  filter(! lall_desc_full %in% c('Otros','Transacciones no clasificadas'))

# Sacar los que no son paises 
df_comex <- df_comex %>% 
  filter(location_code %in% dicc_argendata$geocodigo)

# Calcular proporcion de cada manufactura 
df_comex <- df_comex %>% 
  mutate(manufacturas = if_else(str_detect(lall_desc_full,'Manufacturas'),'Industrial','No industrial')) %>% 
  filter(manufacturas == 'Industrial') %>% 
  group_by(year,location_code,manufacturas) %>% 
  summarize(exportaciones = sum(expo,na.rm=T)) %>% 
  ungroup() %>%
  group_by(year) %>% 
  mutate(prop = exportaciones / sum(exportaciones,na.rm=T))

# Sumar nombre de pais 
df_comex <- df_comex %>% 
  left_join(dicc_argendata,by=c('location_code'='geocodigo'))
df_comex <- df_comex %>% 
  filter(!is.na(name_long))

# Seleccionar variables 
df_comex <- df_comex %>% 
  select(year,iso3=location_code,name_long,prop_expo = prop)

# guardar resultados 
readr::write_csv(df_comex,file.path(outstub,paste0(output_name,'.csv')))
