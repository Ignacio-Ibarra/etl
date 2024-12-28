#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(2)
fecha_ultima_actualizacion <- as.Date("2024-07-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/magyp_scraper_links.R")


result <- MAGYP.extraer_links_datos_abiertos(page_suffix = "estimaciones-agricolas", h3_target = "Estimaciones agrícolas")

url <- result$url

nombre <- glue::glue("{result$title} - {result$text}")

institucion <- "Subsecretaria de Agricultura. Dirección Nacional de Agricultura. Dirección de Estimaciones Agrícolas"

download_filename <- "estimaciones_agricolas.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desactivo la verificacion de SSL
GET(url, 
    config = config(ssl_verifypeer = FALSE), 
    write_disk(destfile, overwrite = TRUE))


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 296,
                      url = url,
                      nombre = title_raw,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)