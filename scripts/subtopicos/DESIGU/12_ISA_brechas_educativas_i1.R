#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '12_ISA_brechas_educativas_i1.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_brechas_educativas_i1.csv'
id_fuente <- 186
fuente_raw1 <- sprintf("R%sC0",id_fuente)

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]



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


df_output <- readxl::read_excel(argendataR::get_temp_path(fuente_raw1)) %>% 
  janitor::clean_names()

df_output <- df_output %>% 
  rename(anosedu = anoseducacion) %>% 
  pivot_longer(cols = -anosedu, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = pks,
  drop_joined_df = F
)


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    control = comparacion,
    pk =  pks,
    es_serie_tiempo = F,
    # columna_indice_tiempo = ,
    # nivel_agregacion =[DEFINIR],
    # aclaraciones = [DEFINIR],
    etiquetas_indicadores = etiquetas,
    unidades = list("valor" = "unidades")
  )
