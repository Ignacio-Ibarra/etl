#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '1_ISA_nbi-multidimensional_it1-it2.R'
subtopico <- 'POBREZ'
output_name <- 'ISA_nbi-multidimensional_it1-it2.csv'
id_fuente <- 146
fuente_raw1 <- sprintf("R%sC0",id_fuente)

df_output <- readxl::read_excel(argendataR::get_fuente_path(fuente_raw1)) %>% 
  mutate(province = ifelse(province == "PBA", "Interior de PBA", province))

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior %>% 
    mutate(across(c(year, nbi_rate), as.numeric)),
  pk = c('province','year'),
  drop_joined_df = F
)

print(comparacion)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Porcentaje de Hogares con NBI. Provincias y total  país, 1980 - 2010",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('province','year'),
    control = comparacion, 
    es_serie_tiempo = T,
    columna_indice_tiempo = 'year',
    nivel_agregacion = 'pais',
    etiquetas_indicadores = list('nbi_rate' = 'Porcentajes de hogares con NBI'),
    unidades = list('nbi_rate' = 'porcentaje')
  )



