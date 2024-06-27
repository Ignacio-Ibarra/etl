#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '4_ISA_regiones-ipcf_it1.R'
subtopico <- 'SALING'
output_name <- 'ISA_regiones-ipcf_it1.csv'
id_fuente <- 174
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
  pk = c('year','semestre','region','deflactor'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('year','semestre','region','deflactor'),
    control = comparacion, 
    es_serie_tiempo = [DEFINIR],
    columna_indice_tiempo = [DEFINIR],
    nivel_agregacion =[DEFINIR],
    etiquetas_indicadores = list('indice' = 'Valor relativo del ingreso regional respecto al nacional en 2023-I'),
    unidades = list('indice' = 'unidades')
  )
