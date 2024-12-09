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
fecha_ultima_actualizacion <- as.Date("2021-05-05")
fecha_actualizar <- "Sin informacion"

logged_url <- "https://www.iea.org/product/download/013141-000340-013121" # Logged

download_url <- "https://docs.google.com/spreadsheets/d/1vYeyFJ-pWBohcKvHwmk-dohxc-GUNutX/export?format=xlsx"

report_url <- "https://www.iea.org/data-and-statistics/data-product/the-role-of-critical-minerals-in-clean-energy-transitions-2"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "TheRoleofCriticalMineralsinCleanEnergyTransitions.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(download_url, destfile = destfile, mode = "wb")

nombre = "The Role of Critical Minerals in Clean Energy Transitions."
institucion = "International Energy Agency (IEA)"

# agregar_fuente_raw(url = report_url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 279,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)