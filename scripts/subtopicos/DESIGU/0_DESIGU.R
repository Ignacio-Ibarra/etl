fuentes_usadas <- c('R186C0',
                    'R187C0',
                    'R188C0',
                    'R189C0',
                    'R190C0',
                    'R191C0',
                    'R192C0',
                    'R193C0',
                    'R194C0',
                    'R195C0',
                    'R196C0',
                    'R197C0',
                    'R198C0',
                    'R199C0',
                    'R200C0',
                    'R201C0',
                    'R202C0',
                    'R203C0',
                    'R204C0',
                    'R205C0',
                    'R206C0',
                    'R207C0',
                    "R210C0",
                    "R35C76",
                    "R35C78",
                    "R35C79",
                    "R211C77")

scripts_raw <- argendataR::fuentes_raw() %>% 
  dplyr::filter(codigo %in% fuentes_usadas) %>% pull(script) %>% 
  paste0("~/etl/scripts/descarga_fuentes/",.) %>% unique(.)

scripts_clean <- argendataR::fuentes_clean() %>% 
  dplyr::filter(codigo %in% fuentes_usadas) %>% pull(script) %>% 
  paste0("~/etl/scripts/limpieza_fuentes/",.) %>% unique()

scripts_fuentes <- c(scripts_raw, scripts_clean)

n_scripts_fuentes <- length(script_fuentes)

idx <- 1


walk(scripts_fuentes[idx:n_scripts_fuentes], function(x) {
  mensaje_inicio <- paste("Procesando script n°", grep(x, scripts_fuentes), "- Archivo:", x)
  message("##############################################")
  message(mensaje_inicio)
  source(x, local = T)
  idx <- idx + 1 
})

subtopico <-  "DESIGU"
src <- glue::glue("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")
source(src)

entrega <- "datasets_update"
analista <-  c("")
meta_desigu <- metadata("DESIGU")
meta_desigu <- meta_desigu %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)


archivos <- list.files(glue::glue("~/etl/scripts/subtopicos/{subtopico}/"))
scripts <- archivos[grepl("\\.R$", archivos) &
                      ! archivos %in% c(glue::glue("0_{subtopico}.R"), glue::glue("fuentes_{subtopico}.R"))]

n_scripts <- length(scripts)

idx <- 1

walk(scripts[idx:n_scripts], function(x) {
  mensaje_inicio <- paste("Procesando script n°", grep(x, scripts), "- Archivo:", x)
  message("##############################################")
  message(mensaje_inicio)
  source(glue::glue("~/etl/scripts/subtopicos/{subtopico}/{x}"), local = T)
  idx <- idx + 1 
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


