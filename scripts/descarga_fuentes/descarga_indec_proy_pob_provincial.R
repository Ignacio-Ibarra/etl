# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


source("scripts/utils/indec_scraper_links.R")


# Se puede usar esta función para descargar la URL de provincial. 
result <- INDEC.poblacion_proyecciones_nacionales.extraer_links(id = 85, pattern = "c1_proyecciones_prov.*\\.xls$")

url <- result$url

nombre <- result$text

institucion <- "Instituto Nacional de Estadísticas y Censo"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 389,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)