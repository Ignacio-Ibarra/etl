#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '5_ISA_regiones-ipcf_it2.R'
subtopico <- 'SALING'
output_name <- 'ISA_regiones-ipcf_it2.csv'
id_fuente <- 175
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semestre','region'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semestre','region'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semestre'),
    nivel_agregacion ='pais',
    etiquetas_indicadores = list('indice' = 'Valor relativo del ingreso regional respecto al nacional en 2003-II'),
    unidades = list('indice' = 'unidades')
  )
