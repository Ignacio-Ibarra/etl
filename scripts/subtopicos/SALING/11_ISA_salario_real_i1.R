#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '11_ISA_salario_real_i1.R'
subtopico <- 'SALING'
output_name <- 'ISA_salario_real_i1.csv'
id_fuente <- 176
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1)) %>% pivot_longer(-año, names_to = "variable", values_to = "valor") %>% 
  rename(ano = año)

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano','variable'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano','variable'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'ano',
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
