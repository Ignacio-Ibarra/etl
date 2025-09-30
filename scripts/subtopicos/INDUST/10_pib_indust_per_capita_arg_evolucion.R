#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Cargar dato de indec 
source('indust/utils/dato_industria_pib_precios_basicos_indec_2004.R')
# Funcion poblacion
source('indust/utils/estimar_poblacion.R')
# Metadatos 
subtopico <- "INDUST"
output_name <- "10_pib_indust_per_capita_arg_evolucion"
analista <- "Nicolás Sidicaro"
fuente2 <- 'R46C0' # Poblacion WB

# rutas 
outstub <- 'indust/input'

# carga de fuentes - nico 
df_gdp <- readr::read_csv('https://raw.githubusercontent.com/argendatafundar/data/main/ESTPRO/vab_por_sector.csv')

# Filtrar actividad gdp 
df_gdp <- df_gdp %>% 
  filter(letra_desc == 'Industria manufacturera')

# Filtrar 2004 
df_gdp <- df_gdp %>% 
  select(anio,vab) %>% 
  filter(anio < 2004)

# Sumar datos de INDEC 
cuadro3_trimestres_long <- cuadro3_trimestres_long %>% 
  rename(anio=trimestre,vab=valor)
df_gdp <- bind_rows(df_gdp,cuadro3_trimestres_long) 

# Sumar poblcion
df_pop <- poblacion_completa_dato()
df_pop <- df_pop %>% 
  filter(anio >= 1900)

# Calcular GDP Per capita 
df_gdp <- df_gdp %>% 
  left_join(df_pop,by='anio')
df_gdp <- df_gdp %>% 
  mutate(vab_indust_pc = (vab*1000000)/poblacion)
# Armar indice base 1970
df_gdp_aux <- df_gdp %>% 
  filter(anio == 1970) %>% 
  pull(vab_indust_pc)
df_gdp <- df_gdp %>% 
  mutate(vab_indust_pc_base_1970 = (vab_indust_pc / df_gdp_aux)*100)
df_gdp <- df_gdp %>% 
  select(anio,vab_indust_pc_base_1970)

# guardar resultados 
readr::write_csv(df_gdp,file.path(outstub,'10_pib_indust_per_capita_arg_evolucion.csv'))
