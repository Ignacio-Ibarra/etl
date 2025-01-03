#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '17_ISA_tipo_empleo_i2.R'
subtopico <- 'SALING'
output_name <- 'ISA_tipo_empleo_i2.csv'
id_fuente <- 184
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano','mes','variable'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Índice de salarios de INDEC a precios constantes por tipo de empleo. 2016 - 2024",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano','mes','variable'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('ano','mes'),
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
