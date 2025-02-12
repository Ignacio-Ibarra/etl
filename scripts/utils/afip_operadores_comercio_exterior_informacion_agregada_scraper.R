library(curl)


URL_BASE <- "https://www.afip.gob.ar"


URL_CONSULTA <- "https://www.afip.gob.ar/operadoresComercioExterior/informacionAgregada/informacion-agregada.asp"


# Función que descarga todos los links del año requerido
listar_links_descarga <- function(anio){
  
  
  # Obtiene el contenido de la página web
  web_content <- read_html(URL_CONSULTA)
  
  patron <- ".*/operadoresComercioExterior/informacionAgregada/download.aspx.*"
  
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract(., pattern = patron)
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(patron, links)]
  filtered_texts <- text_links[grepl(patron, links)]
  
  meses = str_extract(filtered_texts, "(\\d+)\\/(\\d+)", group = 1)
  anios <- str_extract(filtered_texts, "(\\d+)\\/(\\d+)", group = 2)
  
  result <- data.frame(
      fecha =  glue::glue("{anios}-{meses}-01") %>% as.Date(),
      url = glue::glue("{URL_BASE}{filtered_links}")
    )
  
  links <- result %>%
    dplyr::filter(year(fecha) == anio) %>%
    arrange(fecha) %>% pull(url)
  
  return(links)
}

# Función que descarga los archivos de las URLs - Posee progresión de descarga
descargar_archivos_zip <- function(urls, output_dir = tempdir()) {
  
  filenames <- str_extract(urls, "\\d{6}\\.zip$")
  destfiles <- glue::glue("{tempdir()}/{filenames}")
  
  response <- multi_download(urls 
                             ,destfiles = destfiles 
                             ,progress = TRUE 
                             )
  
  return(destfiles)
  
}

#Función que descomprime todos los archivos zip
descomprimir_archivos <- function(paths, exdir = tempdir(), pattern_archivo){
  
  archivos_descomprimidos <- list()
  
  for (destfile in paths){
    
    zipfilename <- str_extract(destfile, "\\d{6}\\.zip$")
    tryCatch(
      {
        filename <- unzip(destfile, list = T) %>% dplyr::filter(grepl(pattern_archivo, Name)) %>% pull(Name)
        
        unzip(destfile, files = filename, exdir = exdir)
        
        outpath <- file.path(exdir, filename)
        
        archivos_descomprimidos[[zipfilename]] = list("destfile"  = destfile, "outpath" = outpath)
        }, 
      error = function(e) {
        cat("Error al descomprimir:", zipfilename, "\n")
      }
    )
    
  }
  
  # result <- list(archivos_descomprimidos = archivos_descomprimidos, archivos_fallidos = archivos_fallidos)
  
  result <- archivos_descomprimidos
  
  return(result)
  
}

# Lectura para variios archivos de texto
leer_archivo <- function(path){
  ext <- tools::file_ext(path)
  
  if (ext == "csv"){return(read.csv(path))}
  
  if (ext == "lst"){return(read.csv(path, sep = "'") %>% slice(-1)) }
  
  if (ext == "txt"){return(read.csv(path, sep = " "))}
  
  else{stop("Estas leyendo cualquier cosa")}
}

# Lee todos los archivos juntos. 
compilar_archivos <- function(paths){
  df_completo <- paths %>%
    map_dfr(~ leer_archivo(.))
  
  return(df_completo)
}


# Le pasás el año y hace toda la magia. 
afip_comex_info_agregada.obtener_datos_expo = function(anio, intentos_maximos = 3){
  
  pattern_archivo = "total_expo_agregado.*"
  
  urls_iniciales <- listar_links_descarga(anio = anio)
  
  cat("Links encontrados!!\n\n")
  
  descargado <- data.frame(
    url = urls_iniciales,
    path = str_extract(urls_iniciales, "\\d{6}\\.zip$") %>% file.path(tempdir(), .),
    downloaded = FALSE,
    uncompressed = FALSE,
    outpath = NA
  )
  
  a_descargar <- descargado
  
  intentos <- 0
  
  while (nrow(a_descargar)>0 & intentos < intentos_maximos){
    
    urls <-  a_descargar %>% dplyr::filter(!uncompressed) %>% pull(url)
  
    destfiles <- descargar_archivos_zip(urls)
    
    descargado[descargado$path %in% destfiles, c("downloaded")] = TRUE
    
    resultado_descomprimir <- descomprimir_archivos(paths = destfiles, pattern_archivo = pattern_archivo)
    
    destfiles_descomprimidos_ok <- purrr::map_chr(resultado_descomprimir, 
                                  function(x){x$destfile}) %>% unname()
    
    outpaths_ok <- purrr::map_chr(resultado_descomprimir, 
                                  function(x){x$outpath}) %>% unname()
    
    descargado[descargado$path %in% destfiles_descomprimidos_ok, c("uncompressed")] = TRUE
    
    descargado[descargado$path %in% destfiles_descomprimidos_ok, c("outpath")] = outpaths_ok
    
    a_descargar <- descargado %>% filter(!uncompressed)
    
    intentos <- intentos + 1
    
    
  }
  
  if (nrow(a_descargar) == 0){
    
    df <- compilar_archivos(paths = descargado$outpath)
    
    cat("Se compilaron todos los archvios del año: ", anio)
    
    return(df)
    
  }
  if (intentos_maximos  == intentos){
    
    str_error <- glue::glue("No se pudo descromprimir \n\n {a_descargar}")
    stop(str_error)
  }

}

# anio <- 2023
# df_2023 <- afip_comex_info_agregada.obtener_datos_expo(anio = anio)

