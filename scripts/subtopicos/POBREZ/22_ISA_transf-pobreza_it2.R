#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '22_ISA_transf-pobreza_it2.R'
subtopico <- 'POBREZ'
output_name <- 'ISA_transf-pobreza_it2.csv'
id_fuente <- 153
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_fuente_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semester','poverty_line','pov_type'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Impacto de las transferencias estatales en tasas de pobreza e indigencia. 2003-2023",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semester','poverty_line','pov_type'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semester'),
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('poverty_rate' = 'Tasa de indigencia/pobreza (% de personas)'),
    unidades = list('poverty_rate' = 'porcentaje')
  )
