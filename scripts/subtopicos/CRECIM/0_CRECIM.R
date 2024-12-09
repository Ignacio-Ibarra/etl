
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

subtopico <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1) %>% str_extract(., "0_(.*)\\.R", group =1)
src <- glue::glue("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")
source(src)

entrega <- "primera_entrega"
analista <-  c("")
metadatos <- metadata(subtopico)
metadatos <- metadatos %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)


archivos <- list.files(glue::glue("~/etl/scripts/subtopicos/{subtopico}/"))
scripts <- archivos[grepl("\\.R$", archivos) &
                      ! archivos %in% c(glue::glue("0_{subtopico}.R"), glue::glue("fuentes_{subtopico}.R"))]

walk(scripts[1:length(scripts)], function(x) {
  mensaje_inicio <- paste("Procesando script n°", grep(x, scripts), "- Archivo:", x)
  message("##############################################")
  message(mensaje_inicio)
  source(glue::glue("~/etl/scripts/subtopicos/{subtopico}/{x}"), local = T)
})

salidas <- list.files(tempdir(), full.names = T)[list.files(tempdir()) %in% subtopico_outputs(subtopico_nombre = subtopico,
                                                                                              entrega_subtopico = entrega)$name]
salidas <- c(salidas, gsub("\\.csv$", ".json", salidas))

path_data <- glue::glue("~/data/{subtopico}")

purrr::walk(salidas, 
            function (x) {
              
              file.copy(from = x, to = path_data, overwrite = T) 
              message(glue::glue("{x} copiado a {path_data}."))
            })