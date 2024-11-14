
#limpio la memoria
rm( list=ls())  #Borro todos los objetos
gc()   #Garbage Collection

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

source("scripts/utils/st_lous_fed_api.R")

result_metadata <- FRED.get_series_metadata('CPIAUCSL')
result_data <- FRED.get_series_observations('CPIAUCSL')

periodicidad <- months(1)
fecha_ultima_actualizacion <- result_metadata$data$seriess$last_updated %>% as.Date(.)
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- result_data$url

df_raw <- result_data$data$observations

download_filename <- "consumer_price_index_monthly_data.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

nombre <- result_metadata$data$seriess$title
institucion = "Federal Reserve Bank of St. Louis"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 273,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)