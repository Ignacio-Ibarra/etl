# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


source("scripts/utils/indec_scraper_links.R")

result <- INDEC.balanza_pagos.extraer_links(id=45, pattern = ".*Base_servicios_internacionales_pais_CABPS.*\\.csv")

url <- result$url

nombre <- result$text


download_filename <- "indec_estimacion_experimental_servicios_internacionales_CABPS.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = "INDEC",
#                    nombre = "Balanza de pagos: Exportaciones y balance",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 95,
                      url = url, 
                      nombre = nombre, 
                      institucion = "Instituto Nacional de Estadísticas y Censos",
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
