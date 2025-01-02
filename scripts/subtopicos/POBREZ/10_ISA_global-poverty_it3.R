#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '10_ISA_global-poverty_it3.R'
subtopico <- 'POBREZ'
output_name <- 'ISA_global-poverty_it3.csv'
id_fuente <- 144
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_fuente_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('region_code','region_name','year','poverty_line'),
  drop_joined_df = F
)

print(comparacion)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Tasas de pobreza por ingresos por año, línea de 6.85 dólares diarios a PPP (2017). Argentina y regiones del mundo.",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('region_code','region_name','year','poverty_line'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'year',
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('poverty_rate' = 'Tasa de pobreza (% de personas)'),
    unidades = list('poverty_rate' = 'porcentaje')
  )
