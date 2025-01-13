#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


code_name <- '23_ISA_vivienda_i1.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_vivienda_i1.csv'
id_fuente <- 206
fuente_raw1 <- sprintf("R%sC0",id_fuente)


meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  filter(str_detect(dataset_archivo, output_name)) %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key, .keep_all = T)



nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw1) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente_raw1)) %>% 
  janitor::clean_names()

df_output <- df_output %>% 
  pivot_longer(-ano, names_to = "variable", values_to = "valor")

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('ano','variable'),
  drop_joined_df = F
)
print(comparacion)

etiquetas <- meta_desigu %>% 
  pull(descripcion) %>% 
  as.list()

names(etiquetas) <- meta_desigu %>% 
  pull(variable_nombre)

pks <- meta_desigu %>% 
  filter(primary_key == "TRUE") %>% 
  pull(variable_nombre)

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Proporción de hogares propietarios de su vivienda por quintil. 1992 -2023",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  pks,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = "ano",
    # nivel_agregacion = ,
    # aclaraciones = [DEFINIR],
    etiquetas_indicadores = etiquetas,
    unidades = list("valor" = "unidades")
  )
