subtopico <-  "ESTPRO"
src <- glue::glue("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")
source(src)

entrega <- "datasets_primera_entrega"
analista <-  c("")
meta_desigu <- metadata(subtopico)
meta_desigu <- meta_desigu %>% 
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
