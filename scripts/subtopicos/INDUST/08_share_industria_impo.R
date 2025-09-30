#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
options(scipen=999) # notacion cientifica
# Metadatos 
subtopico <- "INDUST"
output_name <- "08_share_industria_impo"
analista <- "Nicolás Sidicaro"
fuente1 <- '' #

# rutas 
instub <- 'indust/auxiliar/expo_impo_serie_larga/'
outstub <- 'indust/input'

# Cargar datos 
df_comex <- read_csv(file.path(instub,'importaciones_por_pais_1962_2023.csv'))

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# Filtrar productos no clasificados 
df_comex <- df_comex %>% 
  filter(! lall_desc_full %in% c('Otros','Transacciones no clasificadas'))

# Calcular proporcion de cada manufactura 
df_comex <- df_comex %>% 
  group_by(year,location_code,lall_desc_full) %>% 
  summarize(importaciones = sum(impo)) %>% 
  ungroup() %>%
  group_by(year,location_code) %>% 
  mutate(prop = importaciones / sum(importaciones))

# Sumar nombre de pais 
df_comex <- df_comex %>% 
  left_join(dicc_argendata,by=c('location_code'='geocodigo'))
df_comex <- df_comex %>% 
  filter(!is.na(name_long))

# guardar resultados 
readr::write_csv(df_comex,file.path(outstub,paste0(output_name,'.csv')))
