#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '14_ISA_salarios_mundo_i1.R'
subtopico <- 'SALING'
output_name <- 'ISA_salarios_mundo_i1.csv'
id_fuente <- 179
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1))
colnames(df_output) <- str_replace(colnames(df_output), " ", "")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('pais'),
  drop_joined_df = F
)

print(comparacion)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Ingreso laboral horario en América Latina. 2021 o año más reciente.",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('pais'),
    control = comparacion, 
    es_serie_tiempo = F,
    nivel_agregacion ="paises",
    etiquetas_indicadores = list('salariohorario' = 'Valor promedio por país del ingreso laboral por hora de la ocupación principal de todos los trabajadores expresados en dólares a paridad de poder de compra (PPP 2011)'),
    unidades = list('salariohorario' = 'unidades')
  )
