#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()


meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)


code_name <- '18_ISA_regiones_i2.R'
subtopico <- 'DESIGU'
output_name <- 'ISA_regiones_i2.csv'
id_fuente <- 205
fuente_raw1 <- sprintf("R%sC0",id_fuente)

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
    aclaraciones = "Coeficiente de variación del ingreso entre regiones y brecha de ingresos entre GBA y Noreste. 1992 - 2023",
    subtopico = subtopico,
    fuentes = c(fuente_raw1),
    analista = "",
    pk =  pks,
    control = comparacion,
    es_serie_tiempo = T,
    columna_indice_tiempo = 'ano',
    # nivel_agregacion =[DEFINIR],
    # aclaraciones = [DEFINIR],
    etiquetas_indicadores = etiquetas,
    unidades = list("valor" = "unidades")
  )

output_name <- gsub("\\.csv", "",output_name)

mandar_data(paste0(output_name, ".csv"), subtopico = "DESIGU", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "DESIGU",  branch = "dev")

