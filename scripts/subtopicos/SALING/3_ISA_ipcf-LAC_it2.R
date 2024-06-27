#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '3_ISA_ipcf-LAC_it2.R'
subtopico <- 'SALING'
output_name <- 'ISA_ipcf-LAC_it2.csv'
id_fuente <- 172
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
  pk = c('country_code','year'),
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  c('country_code','year'),
    control = comparacion, 
    es_serie_tiempo = [DEFINIR],
    columna_indice_tiempo = [DEFINIR],
    nivel_agregacion =[DEFINIR],
    etiquetas_indicadores = list('ipcf_promedio' = 'Ingreso per cápita familiar promedio en dólares a PPA 2017'),
    unidades = list('ipcf_promedio' = 'unidades')
  )
