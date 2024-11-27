
buscar_codigos_fuentes <- function(ruta_script) {
  tryCatch({
    # Leer el contenido del script como texto
    lineas <- readLines(ruta_script, warn = FALSE)
    
    # Unir todas las líneas en un único string para procesar
    texto <- paste(lineas, collapse = "\n")
    
    # Buscar todas las coincidencias con el patrón
    patron <- "\\bfuente.*\\b\\s*(<-|=)\\s*(.+)"
    match <- str_match_all(texto, patron)[[1]]
    
    # Si no hay coincidencias, devolver un vector vacío
    if (is.null(match) || nrow(match) == 0) {
      return(character(0))
    }
    
    # Extraer la columna con los valores asignados (tercera columna)
    valores <- match[, 3]
    
    # Filtrar solo los códigos que cumplen "^R\\d+C\\d+"
    valores_limpios <- str_replace_all(valores, '"', "")
    patron_codigos <- "^R\\d+C\\d+"
    codigos <- valores_limpios[str_detect(valores_limpios, patron_codigos)]
    
    return(codigos)
  }, error = function(e) {
    message(glue::glue("Error procesando el archivo {ruta_script}: {e$message}"))
    return(character(0))
  })
}


escribir_fuentes_subtopico <- function(subtopico){
  
  folder_subtopico <- glue::glue("scripts/subtopicos/{subtopico}")
  scripts <- list.files(folder_subtopico)
  patron <- glue::glue(".*{subtopico}.*")
  scripts_revisar <- scripts[!grepl(patron,scripts)]
  
  # Variables que quieres buscar
  
  # Ruta al script
  rutas <- file.path(folder_subtopico, scripts_revisar)
  
  descarga_fuentes_folder <- "scripts/descarga_fuentes"
  limpieza_fuentes_folder <- "scripts/limpieza_fuentes"
  
  fuentes_utilizadas <- purrr::map(rutas, buscar_codigos_fuentes) %>% unlist() %>% unique()
  
  db_fclean <- fuentes_clean() %>% dplyr::filter(codigo %in% fuentes_utilizadas)
  
  scripts_clean <- db_fclean$script %>% file.path(limpieza_fuentes_folder, .)
  
  fraw_clean <- db_fclean$id_fuente_raw %>% 
    paste0("R",.,"C0")
  
  scripts_raw <- fuentes_raw() %>% 
    dplyr::filter(codigo %in% c(fuentes_utilizadas, fraw_clean)) %>% 
    drop_na(script) %>% 
    pull(script) %>% 
    file.path(descarga_fuentes_folder, .)
  
  
  # Tienen que ir en este orden los scripts
  sources <- c(scripts_raw, scripts_clean)
  
  lines_to_write <- glue::glue("source('{sources}')")
  
  # Armo el script fuentes_SUBTOP.R pero de manera automática. 
  
  script_name <- glue::glue("fuentes_{subtopico}.R")
  
  script_path <- file.path(folder_subtopico, script_name)
  
  if (file.exists(script_path)){file.remove(script_path)}
  
  stringi::stri_write_lines(lines_to_write, con = script_path)
  
}


# Leer argumentos desde la línea de comandos
args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0) {
  stop("Por favor, proporciona un subtopico como argumento.")
}

subtop <- args[1]
escribir_fuentes_subtopico(subtopico = subtop)
str_message <- glue::glue("Se ha creado el archivo fuentes_{subtop}.R")
message(str_message)



