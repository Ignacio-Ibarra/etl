library(jsonlite)

URL_BASE = "https://www.presupuestoabierto.gob.ar/sici/rest-api/catalog/site"

PRESUPUESTO.get_catalog <- function(){
  jsonlite::fromJSON(URL_BASE)$distribuciones
}


PRESUPUESTO.get_data <- function(ejercicio, urlArchivo){
  
  file <- tempfile(fileext = ".zip")
  
  # Descargar el archivo ZIP
  GET(urlArchivo, write_disk(file, overwrite = TRUE))
  
  # Verificar si el archivo ZIP se descargó correctamente
  if (file.exists(file)) {
    
    csv_filename <- unzip(file, list = T) %>% dplyr::filter(Length == max(Length)) %>% pull(Name)
    # Descomprimir el archivo ZIP
    unzip(file, files = csv_filename, exdir = tempdir())
    
    # Leer el archivo CSV dentro del directorio descomprimido
    csv_filepath <- file.path(tempdir(), csv_filename)
    
    tryCatch({
      data <- fread(csv_filepath, header = TRUE, quote = "\"", sep = ",")
      if (!is.null(data)) {
        print(paste("Archivo leído correctamente para el ejercicio", ejercicio))
        return(data)
      } else {
        print(paste("Error: Archivo vacío para el ejercicio", ejercicio))
      }
    }, error = function(e) {
      cat("Error al leer el archivo para el ejericico", ejercicio, ":", e$message, "\n")
    })
  } else {
    cat("El archivo ZIP no se descargó correctamente para el ejericico", ejercicio, "\n")
  }
  
}
