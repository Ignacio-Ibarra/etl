#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '19_ISA_etario-pobreza_it1.R'
subtopico <- 'POBREZ'
output_name <- 'ISA_etario-pobreza_it1.csv'
id_fuente <- 138
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

df_output <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1)) 

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('year','semester','age_group','poverty_line'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semester','age_group','poverty_line'),
    es_serie_tiempo = T,
    columna_indice_tiempo = c('year','semester'),
    nivel_agregacion = 'pais',
    aclaraciones = NULL,
    etiquetas_indicadores = list('poverty_rate' = 'Tasa de pobreza'),
    unidades = list('poverty_rate' = 'porcentaje')
  )
