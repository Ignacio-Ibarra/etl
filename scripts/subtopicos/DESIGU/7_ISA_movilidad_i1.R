#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)



code_name <- '7_ISA_movilidad_i1.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_movilidad_i1.csv'
id_fuente <- 199
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) 

df_output <- df_output %>% 
  mutate(cohorte = gsub("\\[|\\]", "", cohorte))

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('cohorte'),
  drop_joined_df = F
)

etiquetas <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(descripcion) %>% 
  as.list()

names(etiquetas) <- meta_desigu %>% 
  filter(dataset_archivo == output_name) %>% 
  pull(variable_nombre)

pks <- meta_desigu %>% 
  filter(dataset_archivo == output_name & primary_key == "TRUE") %>% 
  pull(variable_nombre)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Indicador de movilidad intergeneracional educativa para cinco diferentes cohortes de nacimiento, entre 1940 y 1989",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    control = comparacion,
    pk =  pks,
    es_serie_tiempo = F,
    # columna_indice_tiempo = [DEFINIR],
    # nivel_agregacion =[DEFINIR],
    # aclaraciones = [DEFINIR],
    etiquetas_indicadores = etiquetas,
    unidades = list("indicadormovilidad" = "unidades")
  )
