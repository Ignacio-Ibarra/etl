#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '20_ISA_salarios_region_i2.R'
subtopico <- 'SALING'
output_name <- 'ISA_salarios_region_i2.csv'
id_fuente <- 182
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% 
  rename(ano_trim = semestre) %>% 
  pivot_longer(-ano_trim, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") 

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano_trim','variable'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano_trim','variable'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'ano_trim',
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
