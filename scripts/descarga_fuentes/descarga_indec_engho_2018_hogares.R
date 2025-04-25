# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(24)
fecha_ultima_actualizacion <- as.Date("2020-12-11")
fecha_actualizar <- "Sin informacion" 


source("scripts/utils/indec_scraper_links.R")

resultado <- INDEC.bases_microdatos(id = 4) %>% 
  dplyr::filter(grepl("engho2018_hogares.zip", url))

url <- resultado$url

nombre <- glue::glue("{resultado$encuesta}. {resultado$titulo}")

institucion <- "Instituto Nacional de Estadística y Censos"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 394,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
