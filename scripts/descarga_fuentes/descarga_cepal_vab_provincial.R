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

periodicidad <- months(0)
fecha_ultima_actualizacion <- as.Date("2022-12-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "cepal_desagregacion_provincial_vab.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Desagregación provincial del valor agregado bruto de la Argentina, base 2004",
#                    institucion = "Comisión Económica para América Latina y el Caribe (CEPAL) y el Ministerio de Economía de la Argentina, a través de la Secretaría de Planificación del Desarrollo y la Competitividad Federal, y el Centro de Estudios para la Producción (CEP XXI) de la Secretaría de Industria y Desarrollo Productivo",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 221,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename
)
