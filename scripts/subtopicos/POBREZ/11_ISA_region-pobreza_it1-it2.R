#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '11_ISA_region-pobreza_it1-it2.R'
subtopico <- 'POBREZ'
output_name <- 'ISA_region-pobreza_it1-it2.csv'
id_fuente <- 151
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_fuente_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semester','region','poverty_line'),
  drop_joined_df = F
)

print(comparacion)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Tasas de indigencia y de pobreza por regiones, 2016-2023",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semester','region','poverty_line'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semester'),
    nivel_agregacion ='pais',
    etiquetas_indicadores = list('poverty_rate' = 'Tasa de indigencia/pobreza (% de personas)'),
    unidades = list('poverty_rate' = 'porcentaje')
  )
