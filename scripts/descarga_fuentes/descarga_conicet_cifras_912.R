# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  


source("scripts/utils/scraper_conicet_cifras.R")


dataset_id = 912

resultado <- CONICET_CIFRAS.listar_paginas() %>% 
  dplyr::filter(id_dataset == dataset_id) 


url <- CONICET_CIFRAS.obtener_dataset_url(dataset_id = 912)


nombre <- glue::glue("Base de Datos del CONICET - {resultado$h2}. {resultado$h3}. {resultado$texto}")

institucion <- "Consejo Nacional de Investigaciones Científicas y Técnicas"

download_filename <- glue::glue("{basename(url)}.xlsx")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 354,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
