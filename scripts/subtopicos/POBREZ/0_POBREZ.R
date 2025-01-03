subtopico <-  "POBREZ"
# src <- glue::glue("scripts/subtopicos/{subtopico}/fuentes_{subtopico}.R")
# source(src)

# entrega <- "datasets_update"
analista <-  c("")
meta_desigu <- metadata(subtopico)
meta_desigu <- meta_desigu %>% 
  distinct(dataset_archivo, variable_nombre, descripcion, primary_key)


archivos <- list.files(glue::glue("~/etl/scripts/subtopicos/{subtopico}/"))
scripts <- archivos[grepl("\\.R$", archivos) &
                      ! archivos %in% c(glue::glue("0_{subtopico}.R"), glue::glue("fuentes_{subtopico}.R"))]

n_scripts <- length(scripts)

idx <- 1

for (i in idx:n_scripts) {
  
  local_env <- new.env()
  x <- scripts[i]
  mensaje_inicio <- paste("Procesando script n°", grep(x, scripts), "- Archivo:", x)
  message("##############################################")
  message(mensaje_inicio)
  source(glue::glue("~/etl/scripts/subtopicos/{subtopico}/{x}"), local = local_env)
  idx <- idx + 1 
  
}

repo <- gh::gh(glue::glue("/repos/argendatafundar/data/git/trees/main?recursive=true"))

tree <- repo$tree

paths <- sapply(tree, function(x) x$path)

paths <- paths[grepl(glue::glue("{subtopico}/"),paths)]

paths <- paths[grepl("\\.json$|\\.csv$", paths, perl = T)]

files_gh <- gsub(glue::glue("{subtopico}/"), "", paths)

salidas <- list.files(tempdir())

all(files_gh %in% salidas)

files_gh[! files_gh %in% salidas]

# ojo si hay dataset nuevos
salidas <- salidas[salidas %in% files_gh]

salidas_json <- grep("\\.json$",salidas, value = T)

controles <- list()

for (i in salidas_json) {
  
  x <- jsonlite::read_json(glue::glue("{tempdir()}/{i}"))
  controles[[i]] <- x$control
}

path_temp <- glue::glue("~/etl/tmp/{subtopico}_{Sys.time()}.txt")

cat(glue::glue("# {subtopico} \n\n"),
    file = path_temp)



for (i in 1:length(controles)) {
  cat("##", names(controles[i]), append = T, file = path_temp)
  cat("\n", append = T, file = path_temp)
  cat("Nuevas filas: ",unlist(controles[[i]]$diferencia_nfilas), "\n", append = T, file = path_temp)
  
  vars <- names(controles[[i]][["comparacion_cols"]])
  
  for (j in vars) {
    
    checks <- controles[[i]][["comparacion_cols"]][[vars]] %>% 
      names()
    
    cat("## ", j, "\n", append = T, file = path_temp)
    
    for(k in checks) {
      
      if (!grepl("filas", k)) {
        
        cat("### ",  k, ":", unlist(controles[[i]][["comparacion_cols"]][[vars]][[k]]), "\n", append = T, file = path_temp)
        
      } else {
        
        cat("### ",  k, ":", "\n", append = T, file = path_temp)
        
        d <- utils::capture.output(str(bind_rows(controles[[i]][["comparacion_cols"]][[vars]][[k]]))) %>% 
          paste(collapse = "\n")
        
        cat(d,
            "\n", append = T, file = path_temp)
        
      }
      
      
    } 
    
  }
  
  
  cat("\n\n", append = T, file = path_temp)
}

path_data <- glue::glue("~/data/{subtopico}")

purrr::walk(salidas, 
            function (x) {
              
              ok <- file.copy(from = glue::glue("{tempdir()}/{x}"), to = path_data, overwrite = T) 
              
              print(ok) 
              if(ok) {
                message(glue::glue("{x} copiado a {path_data}."))
                
              }
            })




