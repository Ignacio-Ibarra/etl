# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(googlesheets4)


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


source("scripts/utils/scraper_cancilleria_cei.R")


result <- CANCILLERIA.centro_economia_internacional_links() %>% 
  dplyr::filter(text == "1.4.19 Servicios - Exportaciones por principales categorías")

url <- result$href

nombre <- result$text
  
institucion <- "Centro de Economía Internacional. Ministerio de Relaciones Exteriores, Comercio Internacional y Culto"


download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")


download.file(url, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F)

actualizar_fuente_raw(id_fuente = 478,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)