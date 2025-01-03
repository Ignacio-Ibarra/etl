#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '23_ISA_edad_genero_i3.R'
subtopico <- 'SALING'
output_name <- 'ISA_edad_genero_i3.csv'
id_fuente <- 170
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% 
  rename(ano = año) %>% 
  pivot_longer(-ano, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(variable = ifelse(variable == "(65-)", "(65 +)", variable))

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano','variable'),
  drop_joined_df = F
)

print(comparacion)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Evolución de salario real por grupos etarios y género en términos reales. 1992 -2024",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('ano','variable'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'ano',
    nivel_agregacion ='pais',
    etiquetas_indicadores = list('valor' = 'Valor que toma la variable considerada'),
    unidades = list('valor' = 'unidades')
  )
