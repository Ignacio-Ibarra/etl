code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-04-01")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

source("scripts/utils/mteyss_oede_scraper_links.R")

resultado <- mteyss.oede.boletin_estadisticas_laborales_segun_sexo()

url <- resultado$link

download_filename <- "boletin-de-estadisticas-laborales-segun-sexo.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

nombre <- glue::glue("OEDE - {resultado$h3} - {resultado$fecha_publicacion}")

# agregar_fuente_raw(url = url,
#                    institucion = "Ministerio de Trabajo, Empleo y Seguridad Social. Subsecretaría de Planificación, Estudios y Estadísticas. Dirección General de Estudios y Estadísticas Laborales",
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 235,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, api = F,
                      script = code_name)
