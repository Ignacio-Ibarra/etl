# especializacion por rama 

# librerias
library(tidyverse)

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "19_complejidad_vab"
analista <- "Nicolás Sidicaro"
fuente1 <- 'tiva2023_indust_by_country_desagregada.csv' # TiVA 2023 rama industria

# rutas 
aux <- 'indust/auxiliar'
outstub <- 'indust/input'
instub <- 'indust/raw'

# Cargar datos 
df <- read_csv(file.path(instub,fuente1))
df <- janitor::clean_names(df)

# Cargar diccionario
tiva_dicc <- readstata13::read.dta13(file.path(aux,'tiva tim descriptores ordenados.dta'))
tiva_dicc <- tiva_dicc %>% 
  filter(letra == 'C') %>% 
  select(ind_tiva,intensidad_id_ocde) 

# seleccionar variables 
df <- df %>% 
  select(-matches('x[0-9]+'))

# Filtrar menores de 0 
df <- df %>% 
  filter(obs_value > 0)

# Sacar actividades que no interesan 
df <- df %>% 
  filter(!activity %in% c('_T','BTE'))

# seleccionar variables 
df <- df %>% 
  select(time_period,ref_area,activity,obs_value)

# Agregar diccionario
df <- df %>% 
  left_join(tiva_dicc,by=c('activity'='ind_tiva'))
# Filtrar las que no tienen diccionario
df <- df %>% 
  filter(!is.na(intensidad_id_ocde))
# Calcular vab por nivel de complejidad
df <- df %>% 
  group_by(time_period,ref_area,intensidad_id_ocde) %>% 
  summarize(vab = sum(obs_value))
# Calcular share de cada nivel de complejidad 
df <- df %>% 
  group_by(time_period,ref_area) %>% 
  mutate(prop_vab = vab / sum(vab))

# JSON Argendata 
dicc_argendata <- jsonlite::fromJSON('https://raw.githubusercontent.com/argendatafundar/geonomencladores/refs/heads/main/geonomenclador.json')
dicc_argendata <- dicc_argendata %>% 
  select(geocodigo,name_long)

# Agregar pais 
df <- df %>% 
  mutate(ref_area = if_else(ref_area == 'W','WLD',ref_area)) %>% 
  left_join(dicc_argendata,by=c('ref_area'='geocodigo')) %>% 
  rename(iso3 = ref_area)

# Guardar 
readr::write_csv(df,file.path(outstub,paste0(output_name,'.csv')))
