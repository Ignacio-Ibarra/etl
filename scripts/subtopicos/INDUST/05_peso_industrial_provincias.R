#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "05_peso_industrial_provincias"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R221C92' # PBG CEPAL CEP

# rutas 
outstub <- 'indust/input'
instub <- 'indust/raw'

# carga de fuentes - Argendata 
# df_pbg <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()

# carga de fuentes - nico 
df_pbg <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/cepal_desagregacion_provincial_vab_por_sector_CLEAN.parquet')

# armar datos pbg 
seleccionar_industrias_pbg <- df_pbg %>% 
  select(sector_de_actividad_economica) %>% 
  distinct() %>%  
  filter(row_number() > 5 & row_number() < 30) %>% 
  pull(sector_de_actividad_economica)
df_pbg <- df_pbg %>% 
  filter(sector_de_actividad_economica != 'Total sectores')
df_pbg <- df_pbg %>% 
  mutate(industria = if_else(sector_de_actividad_economica %in% seleccionar_industrias_pbg,'industria','otro'))
df_pbg <- df_pbg %>% 
  filter(provincia != 'No distribuido')
df_pbg <- df_pbg %>% 
  group_by(anio,provincia,provincia_id,industria) %>% 
  summarize(vab_pb = sum(vab_pb))
df_pbg <- df_pbg %>% 
  group_by(anio,provincia) %>% 
  mutate(prop = vab_pb / sum(vab_pb))
df_pbg<- df_pbg %>% 
  filter(industria == 'industria')
df_pbg <- df_pbg %>% 
  select(anio,provincia_id,provincia,prop)

# guardar resultados 
readr::write_csv(df_pbg,file.path(outstub,paste0(output_name,'.csv')))
