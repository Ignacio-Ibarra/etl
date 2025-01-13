#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '4_ISA_regiones-ipcf_it1.R'
subtopico <- 'SALING'
output_name <- 'ISA_regiones-ipcf_it1.csv'
id_fuente <- 174
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semestre','region','deflactor'),
  drop_joined_df = F
)

print(comparacion)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Ingreso per cápita familiar relativo al promedio nacional, 2024",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semestre','region','deflactor'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semestre'),
    nivel_agregacion = 'regiones',
    etiquetas_indicadores = list('indice' = 'Valor relativo del ingreso regional respecto al nacional en 2023-I'),
    unidades = list('indice' = 'unidades')
  )
