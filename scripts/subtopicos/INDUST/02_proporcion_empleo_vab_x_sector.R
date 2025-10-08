# Proporcion del empleo y el vab explicado por la industria vs otros sectores 

# Librerias
library(tidyverse)

# Rutas 
outstub <- 'indust/input'
funciones <- 'indust/utils'

# Cargar funciones
source(file.path(funciones,'02a_prop_empleo_industria_arg.R'))
source(file.path(funciones,'02b_prop_vab_industria_arg.R'))

# Leer los archivos generados por los scripts
df_empleo <- prop_empleo()
df_vab <- prop_vab()

# Calcular proporción de empleo por sector para 2024
df_empleo <- df_empleo %>% 
  group_by(anio) %>% 
  mutate(prop_empleo = puestos / sum(puestos)) %>% 
  ungroup() %>% 
  select(anio,letra, letra_desc, prop_empleo)

# Filtrar VAB para 2024
df_vab_2024 <- df_vab %>% 
  select(anio,letra, letra_desc, prop_vab = prop)

# Joinear datos 
df_final <- df_vab %>% 
  left_join(df_empleo,by=c('anio','letra','letra_desc'))

# Filtrar los nulos 
df_final <- df_final %>% 
  filter(!is.na(prop_empleo) & !is.na(prop_empleo))

# Guardar resultados 
write_csv(df_final,file.path(outstub,'proporcion_empleo_vab_x_sector.csv'))
