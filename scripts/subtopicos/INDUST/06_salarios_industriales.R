#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "06_salarios_industriales"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R238C146' # Descriptor 
fuente2 <- 'R239C182' # salarios
fuente3 <- 'R238C138' # puestos
fuente4 <- 'R238C111' # salario_letra

# rutas 
outstub <- 'indust/input'
auxiliar <- 'indust/auxiliar'
# carga de fuentes - Argendata 
# df_dicc <- argendataR::get_clean_path(fuente1) %>% 
#      arrow::read_parquet()
# df_wage <- argendataR::get_clean_path(fuente2) %>% 
#   arrow::read_parquet()
# df_puestos <- argendataR::get_clean_path(fuente3) %>% 
#   arrow::read_parquet()
# df_sal_letra <- argendataR::get_clean_path(fuente4) %>% 
#   arrow::read_parquet()
# carga de fuentes - nico 
df_dicc <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/descriptor_completo_de_actividad_codigo_industrial_internacional_uniforme_ciiu_revision_3_2_digitos_CLEAN.parquet')
df_wage <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/caracterizacion_y_evolucion_de_las_remuneraciones_de_los_trabajadores_registrados_serie_anualxlsx_c_6_CLEAN.parquet')
df_puestos <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/caracterizacion_y_evolucion_del_empleo_serie_anualxlsx_c_5_CLEAN.parquet')
df_sal_letra <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/caracterizacion_y_evolucion_de_las_remuneraciones_de_los_trabajadores_registrados_serie_anualxlsx_c_2_CLEAN.parquet')

# Sectores resumidos
diccionario_sectores <- read_csv(file.path(auxiliar,'sectores_resumidos_industria.txt'))
# Filtrar diccionario 
df_dicc <- df_dicc %>% 
  filter(digitos == 2)
df_dicc$digitos <- NULL

# Salario de los privados 
df_sal_privados <- df_wage %>% 
  filter(rama_de_actividad == 'Total')

# Pasar a dos digitos salarios y puestos
df_2_dig1 <- df_wage %>% 
  select(ciiu_rev3_4d,anio,salario_promedio_puestos_privados)
df_2_dig2 <- df_puestos %>% 
  select(ciiu_rev3_4d,anio,cant_promedio_puestos_privados)
df_2_dig <- df_2_dig1 %>% 
  left_join(df_2_dig2,by=c('ciiu_rev3_4d','anio'))
df_2_dig <- df_2_dig %>% 
  mutate(ciiu_rev3_4d = str_pad(ciiu_rev3_4d,4,pad = '0',side='left'),
         ciiu_rev3_4d = str_extract(ciiu_rev3_4d,'^[0-9]{2}'),
         ciiu_rev3_4d = as.numeric(ciiu_rev3_4d))

df_2_dig <- df_2_dig %>% 
  filter(!is.na(cant_promedio_puestos_privados))

df_2_dig <- df_2_dig %>% 
  group_by(anio,ciiu_rev3_4d) %>% 
  summarize(salario_ponderado = weighted.mean(salario_promedio_puestos_privados,cant_promedio_puestos_privados))

df_2_dig <- df_2_dig %>% 
  left_join(df_dicc,by=c('ciiu_rev3_4d'='codigo'))

# Filtrar por codigos industriales 
df_2_dig <- df_2_dig %>% 
  filter(ciiu_rev3_4d %in% c(15:37))

# Agregar salario promedio de la industria 
df_sal_general <- df_sal_letra %>% 
  filter(letra == 'Z')
df_sal_letra <- df_sal_letra %>% 
  filter(letra == 'D')
df_sal_letra <- df_sal_letra %>% 
  mutate(ciiu_rev3_4d = 99,
         descripcion = 'Promedio industria') 
df_sal_letra <- df_sal_letra %>% 
  select(anio,ciiu_rev3_4d,salario_ponderado = salario_promedio_privados,descripcion)
df_2_dig <- df_2_dig %>% 
  bind_rows(df_sal_letra)

# Promedio respecto a salario privados
df_sal_privados <- df_sal_privados %>% 
  select(anio,salario_promedio_puestos_privados)

df_2_dig <- df_2_dig %>% 
  left_join(df_sal_privados,by='anio')

# Calcular promedio con respecto a media 
df_2_dig <- df_2_dig %>% 
  mutate(salario_respecto_media = ((salario_ponderado - salario_promedio_puestos_privados)/salario_promedio_puestos_privados)*100) %>% 
  select(-c(salario_ponderado,salario_promedio_puestos_privados))

# Agregar diccionario corto
df_2_dig <- df_2_dig %>% 
  left_join(diccionario_sectores %>% select(descripcion_original,descripcion_corta),
            by=c('descripcion'='descripcion_original'))

# guardar resultados 
readr::write_csv(df_2_dig,file.path(outstub,paste0(output_name,'.csv')))
