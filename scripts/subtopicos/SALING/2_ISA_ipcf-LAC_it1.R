#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '2_ISA_ipcf-LAC_it1.R'
subtopico <- 'SALING'
output_name <- 'ISA_ipcf-LAC_it1.csv'
id_fuente <- 171
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('country_code','year'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('country_code','year'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'year',
    nivel_agregacion ='paises',
    etiquetas_indicadores = list('ipcf_promedio' = 'Ingreso per cápita familiar promedio en dólares a PPA 2017'),
    unidades = list('ipcf_promedio' = 'unidades')
  )
