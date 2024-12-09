
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

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2024-11-15")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/world_bank_commodity_markets_scraper.R")

pattern <- ".*CMO-Historical-Data-Monthly\\.xlsx"

url <- WorldBank.CommodityMarkets.extraer_links(pattern = pattern)

nombre <- "Commodity Markets. Pink Sheet. Monthly Data"

institucion <- "World Bank"

download_filename <- "CMO-Historical-Data-Monthly.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 271,
                      url = url,
                      nombre = nombre, 
                      institucion = institucion,
                      script = code_name,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)