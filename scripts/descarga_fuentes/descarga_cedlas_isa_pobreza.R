#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

source("scripts/utils/funciones_descarga_cedlas_desde_metadata.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


# Busco links en la metadata de POBREZ
metadata.df <- argendataR::metadata(subtopico = "POBREZ")

# Me quedo con lo que hay que descargar
to.extract <- metadata.df %>%
  dplyr::filter(grepl("https://www.cedlas.econo.unlp.edu.ar/*", url_path)) %>%
  group_by(dataset_archivo) %>% 
  summarise(url_path = first(url_path),
            titulo = first(titulo_grafico)) %>% 
  mutate(archivo_pagina = str_replace(url_path, "https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/", "")
)

# Me quedo con lo que esta ya en fuentes_raw
fuentes_raw.df <- fuentes_raw() %>% 
  dplyr::filter(path_raw %in% to.extract$archivo_pagina)

subir.cedlas(links.df = to.extract, 
             fuentes_raw.df = fuentes_raw.df,
             directorio_descarga = tempdir(),
             code_name = code_name)
