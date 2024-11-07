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

fecha_actualizar <- "Sin informacion"

source("scripts/utils/imf_api.R")


dataset <- imf.get_available_datasets() %>% dplyr::filter(ids == "PCPS")

database_id <- dataset$ids

database_title <- paste0(dataset$names, " - Metadata")

download_filename <- database_title %>% janitor::make_clean_names() %>% paste0("imf_",., ".json")

destfile <- file.path(tempdir(), download_filename)

result <- imf.get_metadata("PCPS")

url <- result$url

data <- result$data

data %>% jsonlite::write_json(., destfile)

# agregar_fuente_raw(url = url,
#                    nombre = database_title,
#                    institucion = "International Monetary Found",
#                    actualizable = T,
#                    script = code_name,
#                    api = T,
#                    path_raw = download_filename,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 266,
                      url = url,
                      actualizable = T,
                      script = code_name,
                      api = T, 
                      path_raw = download_filename,
                      fecha_actualizar = fecha_actualizar)

