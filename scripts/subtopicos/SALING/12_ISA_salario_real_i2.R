#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '12_ISA_salario_real_i2.R'
subtopico <- 'SALING'
output_name <- 'ISA_salario_real_i2.csv'
id_fuente <- 177
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% rename(c("ano" = "año","indice" ="índice")) %>% 
  fill(ano)

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")
colnames(df_anterior) <- trimws(colnames(df_anterior))

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('mes','ano'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('mes','ano'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = c('mes','ano'),
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('indice' = 'Valor mensual del salario calculado a partir del Índice de Salarios publicado por INDEC, en términos reales, deflactado por el IPC nivel general de INDEC (octubre 2016=100)'),
    unidades = list('indice' = 'unidades')
  )
