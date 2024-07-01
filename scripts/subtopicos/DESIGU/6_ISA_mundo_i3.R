#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '6_ISA_mundo_i3.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_mundo_i3.csv'
id_fuente <- 203
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
  pk = c('ano','variable'),
  drop_joined_df = F
)

etiquetas <- rep("sin especificar", length(colnames(df_output))) %>% 
  as.list()
names(etiquetas) <- colnames(df_output)

unidades <- rep("sin especificar", length(colnames(df_output))) %>% 
  as.list()
names(unidades) <- colnames(df_output)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    control = comparacion,
    pk =  c('ano','variable'),
    es_serie_tiempo = T,
    columna_indice_tiempo = ano,
    # nivel_agregacion =[DEFINIR],
    # aclaraciones = [DEFINIR],
    etiquetas_indicadores = etiquetas,
    unidades = unidades
  )
