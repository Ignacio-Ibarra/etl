#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "02a_prop_empleo_industria_arg"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R223C94' # agregados macroeconomicos - cuadro 4

# rutas 
outstub <- 'indust/input'

# nombres sectores 
desc_sector <- read_csv(file.path(outstub,'02a_prop_empleo_industria_arg.csv'))
desc_sector <- desc_sector %>% 
  select(letra,letra_desc) %>% 
  distinct()

# carga de fuentes - Argendata 

# df_am_c4 <- argendataR::get_clean_path(fuente1) %>% 
#   arrow::read_parquet()

# carga de fuentes - nico 
df_am_c4 <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/sh_VBP_VAB_03_25_cuadro_4_CLEAN.parquet')

# seleccionar trimestre y subsector agregados macro
df_am_c4 <- df_am_c4 %>% 
  filter(trimestre == 'Total')
df_am_c4 <- df_am_c4 %>% 
  filter(sub_sector == 'Total sector')

# llevar a formato de empleo (mismos sectores agregados) 
df_am_c4 <- df_am_c4 %>% 
  mutate(sector = if_else(letra_desc_abrev %in% c('Agro','Pesca'),'Agricultura, ganadería, caza y silvicultura + Pesca',letra_desc_abrev),
         letra = if_else(letra %in% c('A','B'),'A_B',letra),
         sector = if_else(letra %in% c('N','O'),'Servicios personales, sociales y de salud',letra_desc_abrev),
         letra = if_else(letra %in% c('N','O'),'N_O',letra))

# Agregar sectores
df_am_c4 <- df_am_c4 %>% 
  group_by(anio,letra) %>% 
  summarize(vab_pb = sum(vab_pb))

# proporcion sectorial anual
df_am_c4 <- df_am_c4 %>% 
  group_by(anio) %>% 
  mutate(prop = vab_pb / sum(vab_pb))

# sumar descripcion
df_am_c4 <- df_am_c4 %>% 
  left_join(desc_sector,by='letra')

# ordenar variables 
df_am_c4 <- df_am_c4 %>% 
  select(anio,letra,letra_desc,vab_pb,prop)

# guardar resultados 
readr::write_csv(df_am_c4,file.path(outstub,'02b_prop_vab_industria_arg.csv'))
