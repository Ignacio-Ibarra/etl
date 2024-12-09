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

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-18")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_scraper_links.R")

agregados_macro_id <- 47

pattern_vbp_vab <- ".*sh_VBP_VAB_.*\\.xls"

url <- INDEC.cuentas_nacionales.extraer_links(id = agregados_macro_id, pattern = pattern_vbp_vab)

download_filename <- url %>% str_extract(., "sh.*\\.xls.*") # esta linea no debería ser así, debería proveer una string estatica

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = "INDEC",
#                    nombre = "Cuentas Nacionales. Agregados Macroeconómicos (PIB). Series por sector de actividad económica: valor bruto de producción y valor agregado bruto, por trimestre",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 223,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name) 