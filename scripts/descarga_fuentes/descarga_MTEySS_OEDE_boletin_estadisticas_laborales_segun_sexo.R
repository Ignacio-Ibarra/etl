# Función para obtener la ruta del archivo, compatible tanto en RStudio como en la consola
get_file_location <- function() {
  # Intenta obtener la ruta del archivo en RStudio
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    return(rstudioapi::getSourceEditorContext()$path)
  }
  
  # Alternativa para obtener la ruta si se usa source()
  this_file <- (function() { attr(body(sys.function(1)), "srcfile") })()
  
  # Si no se obtiene el path (e.g., en consola sin RStudio), asigna un valor por defecto
  if (!is.null(this_file)) {
    return(this_file$filename)
  } else {
    return("Archivo no especificado o ruta predeterminada")
  }
}

code_name <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1)


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
