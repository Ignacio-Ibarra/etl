#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "04_pbg_industrial_distribucion"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R221C92' # PBG CEPAL CEP
fuente2 <- 'pbg_industria_1913_2003.csv' # Libro MinDeP

# rutas 
outstub <- 'indust/input'
instub <- 'indust/raw'

# carga de fuentes - Argendata 
# df_pbg <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()

# carga de fuentes - nico 
df_pbg <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/cepal_desagregacion_provincial_vab_por_sector_CLEAN.parquet')
df_hist <- readr::read_csv(file.path(instub,fuente2))
df_hist <- janitor::clean_names(df_hist)

# armar datos pbg 
seleccionar_industrias_pbg <- df_pbg %>% 
  select(sector_de_actividad_economica) %>% 
  distinct() %>%  
  filter(row_number() > 5 & row_number() < 30) %>% 
  pull(sector_de_actividad_economica)
df_pbg <- df_pbg %>% 
  mutate(industria = if_else(sector_de_actividad_economica %in% seleccionar_industrias_pbg,'industria','otro'))
df_pbg <- df_pbg %>% 
  filter(provincia != 'No distribuido')
df_pbg <- df_pbg %>% 
  group_by(anio,provincia,provincia_id,industria) %>% 
  summarize(vab_pb = sum(vab_pb))
df_pbg <- df_pbg %>% 
  filter(industria == 'industria') %>% 
  group_by(anio) %>% 
  mutate(prop = vab_pb / sum(vab_pb))
df_pbg <- df_pbg %>% 
  select(anio,provincia_id,provincia,prop)

# preparar datos historicos 
df_hist <- df_hist %>% 
  pivot_longer(-provincia,names_to='anio',values_to='prop')
df_hist <- df_hist %>% 
  mutate(anio = as.numeric(str_remove(anio,'^x')))
# homogeneizar nombres de provincias 
df_hist <- df_hist %>% 
  mutate(provincia = case_when(provincia == 'Ciudad de Buenos Aires'~ 'CABA',
                               TRUE ~ provincia))
# juntar datos 
df_final <- bind_rows(df_hist,df_pbg)
df_final$provincia_id <- NULL
aux <- df_pbg %>% ungroup() %>% select(provincia,provincia_id) %>% distinct()
df_final <- df_final %>% 
  left_join(aux,by='provincia')

# ordenar columnas 
df_final <- df_final %>% 
  select(anio,provincia_id,provincia,prop)

# guardar resultados 
readr::write_csv(df_final,file.path(outstub,'04_pbg_industrial_distribucion.csv'))
