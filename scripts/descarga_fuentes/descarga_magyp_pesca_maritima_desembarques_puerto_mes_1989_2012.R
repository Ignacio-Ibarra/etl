#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/magyp_scraper_links.R")

url_general <- "https://www.magyp.gob.ar/sitio/areas/pesca_maritima/"

institucion <- "Secretaría de Agricultura, Ganadería y Pesca. Subsecretaria de Pesca y Acuicultura"

nombre <- "Desembarques por puerto y mes (1989-2012)"

download_filename <- "desembarque_puerto_mes_1989_2012.json"

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desde 1989 a 2012 cada rar viene con un solo archivo y
# solo hay data agregada, por eso hay que elegir pestaña.
# eg Para especie por mes "esp_mes.*"
# Solo hay target desembarque.
lista_raw_1989_2012 <- MAGYP.obtener_datos(target = "desembarque", anios = 1989:2012, patron_archivo = NULL, patron_pestania = "pto_mes.*" )


jsonlite::write_json(lista_raw_1989_2012, path = destfile)


# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 333,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)