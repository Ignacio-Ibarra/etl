code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-04-01")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

source("scripts/utils/mteyss_oede_scraper_links.R")

resultado <- mteyss.oede.remuneracions_empleo_y_empresas()

resultado <- resultado[resultado$detalle == "Caracterización y evolución del empleo (Serie anual)",]

url <- resultado$link

download_filename <- paste0(resultado$detalle %>% janitor::make_clean_names(),".xlsx")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

nombre <- glue::glue("OEDE - {resultado$h3} - {resultado$h4} - {resultado$detalle} - {resultado$fecha_publicacion}")

# agregar_fuente_raw(url = url,
#                    institucion = "Ministerio de Trabajo, Empleo y Seguridad Social. Subsecretaría de Planificación, Estudios y Estadísticas. Dirección General de Estudios y Estadísticas Laborales",
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )



actualizar_fuente_raw(id_fuente = 238,
                      nombre = nombre, # cambia el nombre cada vez que cambia la fecha de publicacion
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, api = F)