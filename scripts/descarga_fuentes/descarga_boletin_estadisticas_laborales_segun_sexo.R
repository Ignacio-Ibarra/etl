# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-04-01")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

source("scripts/utils/boletin_estadisticas_laborales_scraper_links.R")

page_url <- "https://www.argentina.gob.ar/trabajo/estadisticas/boletin-de-estadisticas-laborales-segun-sexo"
url_base <- "https://www.argentina.gob.ar/sites/default/files/"

url = BEL.extraer_links(page_url, url_base)

download_filename <- "boletin-de-estadisticas-laborales-segun-sexo.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)


# agregar_fuente_raw(url = url,
#                    institucion = "Ministerio de Trabajo, Empleo y Seguridad Social. Subsecretaría de Planificación, Estudios y Estadísticas. Dirección General de Estudios y Estadísticas Laborales",
#                    nombre = "Boletín de estadísticas laborales según sexo",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 235,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)