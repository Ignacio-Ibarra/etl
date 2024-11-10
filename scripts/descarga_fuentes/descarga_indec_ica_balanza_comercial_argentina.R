# oferta y demanda global trimestral INDEC cuentas nacionales  -----------

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

source("scripts/utils/indec_scraper_links.R")

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
fecha_ultima_actualizacion <- as.Date("2024-03-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


result <- INDEC.intercambio_comercial_argentino.extraer_links(id = 40, pattern = ".*balan_1910_\\d{4}\\.xls.*")

url <- result$url

nombre <- result$text

download_filename <- nombre %>% janitor::make_clean_names(.) %>% paste0(.,".xls")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = destfile)


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = "INDEC",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 269,
                      url = url,
                      nombre = "Series trimestrales de oferta y demanda globales",
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)