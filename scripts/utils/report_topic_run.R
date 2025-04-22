report_topic <- function(subtopico){
  
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
    if (is.null(names(controles[i]))) {
      cat("##", "NO SE HALLARON CONTROLES", " \n\n", append = T, file = path_temp)
      next
    }

    cat("##", names(controles[i]), " \n\n", append = T, file = path_temp)
    cat(glue::glue("https://github.com/argendatafundar/data/blob/dev/{subtopico}/{gsub('.json$', '.csv', names(controles[i]))}")," \n", append = T, file = path_temp)
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
  
  salidas
}
