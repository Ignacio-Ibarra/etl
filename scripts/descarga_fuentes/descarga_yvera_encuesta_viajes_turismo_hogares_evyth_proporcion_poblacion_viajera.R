# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("./scripts/utils/scraper_yvera.R")

result <- YVERA.datos_abiertos('encuesta-viajes-turismo-hogares-evyth') %>% 
  dplyr::filter(grepl("Proporción de la población que al menos realizó un viaje en el año", texto))

nombre_archivo <- glue::glue("{result$texto}")

url <- result$download_link

institucion <- "Subsecretaría de Turismo. Dirección Nacional de Mercados y Estadística. Ministerio del Interior"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile, 
              method = "curl",
              extra = "-k")

# 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 490,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)