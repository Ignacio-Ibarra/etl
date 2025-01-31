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
descargar_archivos_zip <- function(urls, output_dir = tempdir(), max_retries = 3) {
  
  filenames <- str_extract(urls, "\\d{6}\\.zip$")
  destfiles <- glue::glue("{tempdir()}/{filenames}")
  
  curl::multi_download(urls, destfiles = destfiles, resume = T, progress = T)
  
  return(destfiles)
  
}

#Función que descomprime todos los archivos zip
descomprimir_archivos <- function(paths, exdir = tempdir(), pattern_archivo){
  
  archivos_descomprimidos <- c()
  
  for (destfile in paths){
    
    filename <- unzip(destfile, list = T) %>% filter(grepl(pattern_archivo, Name)) %>% pull(Name)
    
    unzip(destfile, files = filename, exdir = exdir)
    
    outpath <- file.path(exdir, filename)
    
    archivos_descomprimidos <- c(archivos_descomprimidos, outpath)
    
  }
  
  return(archivos_descomprimidos)
  
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
  
  retunr(df_completo)
}


# Le pasás el año y hace toda la magia. 
afip_comex_info_agregada.obtener_datos_expo = function(anio){
  
  pattern_archivo = "total_expo_agregado.*"
  
  urls <- listar_links_descarga(anio = anio)
  
  cat("Links encontrados!!\n\n")
  
  destfiles <- descargar_archivos_zip(urls)
  
  cat("Descarga de archivos mensuales completa!!\n\n")
  
  archivos_descomprimidos <- descomprimir_archivos(paths = destfiles, pattern_archivo = pattern_archivo)
  
  cat("Se completó la descompresión de archivos!!\n\n")
  
  df <- compilar_archivos(paths = archivos_descomprimidos)
  
  cat("Se compilaron todos los archvios del año: ", anio)
  
  return(df)
  
  }



# df_2023 <- afip_comex_info_agregada.obtener_datos_expo()