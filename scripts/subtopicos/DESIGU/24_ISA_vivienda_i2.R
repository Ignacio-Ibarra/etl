#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


code_name <- '24_ISA_vivienda_i2.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_vivienda_i2.csv'
id_fuente <- 207
fuente_raw1 <- sprintf("R%sC0",id_fuente)


meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  filter(str_detect(dataset_archivo, output_name)) %>% 
  distinct(dataset_archivo, variable_nombre,
           descripcion, primary_key, .keep_all = T)



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
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  pks,
    control = comparacion,
    es_serie_tiempo = T,
    columna_indice_tiempo = "ano",
    # nivel_agregacion =[DEFINIR],
    aclaraciones = "Brecha en el acceso a servicios entre quintil 5 y quintil 1. 1992 - 2023",
    etiquetas_indicadores = etiquetas,
    unidades = list("valor" = "unidades")
  )
