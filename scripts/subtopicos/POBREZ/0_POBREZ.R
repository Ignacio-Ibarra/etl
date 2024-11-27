subtopico <-  "POBREZ"
# src <- glue::glue("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")
# source(src)

analista <-  c("")
entrega <- "datasets_primera_entrega"
#-- Sources -----



archivos <- list.files(glue::glue("~/etl/scripts/subtopicos/{subtopico}/"))
scripts <- archivos[grepl("\\.R$", archivos) &
                      ! archivos %in% c(glue::glue("0_{subtopico}.R"), glue::glue("fuentes_{subtopico}.R"))]

walk(scripts[1:length(scripts)], function(x) {
  mensaje_inicio <- paste("Procesando script n°", grep(x, scripts), "- Archivo:", x)
  message("\n##############################################")
  message(mensaje_inicio)
  source(glue::glue("~/etl/scripts/subtopicos/{subtopico}/{x}"), local = T)
})

report_topic(subtopico = subtopico)

limpiar_temps()

salidas <- list.files(tempdir(), full.names = T)[list.files(tempdir()) %in% subtopico_outputs(subtopico_nombre = subtopico,
                                                                                              entrega_subtopico = "datasets_primera_entrega")$name]
salidas <- c(salidas, gsub("\\.csv$", ".json", salidas))

salidas <- unique(salidas)


path_data <- glue::glue("~/data/{subtopico}")

purrr::walk(salidas, 
            function (x) {
              
              ok <- file.copy(from = glue::glue("{tempdir()}/{x}"), to = path_data, overwrite = T) 
              
              print(ok) 
              if(ok) {
                message(glue::glue("{x} copiado a {path_data}."))
                
              }
            })



