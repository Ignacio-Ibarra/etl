#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/magyp_scraper_links.R")

url_general <- "https://www.magyp.gob.ar/sitio/areas/pesca_maritima/desembarques/"

institucion <- "Secretaría de Agricultura, Ganadería y Pesca. Subsecretaria de Pesca y Acuicultura"

nombre <- "Desembarques por puerto, flota, especie y mes (2013-2021)"

download_filename <- "desembarque_puerto_flota_especie_mes_2013_2021.json"

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desde 2013 a 2021 cada zip viene con dos archivos adentros.
# Existe la data desagregada por puerto flota especie mes, por eso hay que elegir patron_archivo
# En caso de elegir la data desagregada no hace falta pasarle patron_pestania. 
# Ademas hay data data agregada, por eso hay que elegir patron_pestania
# eg Para especie por mes "esp_mes.*"
# Solo hay target desembarque. 
lista_raw_2013_2021 <- MAGYP.obtener_datos(target = "desembarque", anios = 2013:2021, patron_archivo = ".*Por puerto.*", patron_pestania = NULL)


jsonlite::write_json(lista_raw_2013_2021, path = destfile)


# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 330,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)