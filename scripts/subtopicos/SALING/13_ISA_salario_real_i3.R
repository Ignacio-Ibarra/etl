#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '13_ISA_salario_real_i3.R'
subtopico <- 'SALING'
output_name <- 'ISA_salario_real_i3.csv'
id_fuente <- 178
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano'),
  drop_joined_df = F
)

print(comparacion)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = "ano",
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('salario' = 'Salario medio real a pesos constantes de 2014'),
    unidades = list('salario' = 'unidades')
  )
