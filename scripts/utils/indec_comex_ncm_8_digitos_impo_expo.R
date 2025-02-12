library(curl)
library(httr)
library(jsonlite)
library(dplyr)

URL_BASE <- "https://comex.indec.gob.ar"


build_url_zip <- function(tipo_comercio = NULL, anio = NULL, modalidad = NULL) {
  
  # Definir valores válidos
  valid_tipo_comercio <- c("exports", "imports")
  valid_anios <- 2002:(as.integer(format(Sys.Date(), "%Y")) - 1)  # Años desde 2002 hasta el año anterior al actual
  valid_modalidad <- c("M", "Y")
  
  # Verificar que los parámetros no sean NULL
  if (is.null(tipo_comercio)) {
    stop("Error: 'tipo_comercio' no puede ser NULL.")
  }
  if (is.null(anio)) {
    stop("Error: 'anio' no puede ser NULL.")
  }
  if (is.null(modalidad)) {
    stop("Error: 'modalidad' no puede ser NULL.")
  }
  
  
  # Comprobaciones de validez
  if (!(tipo_comercio %in% valid_tipo_comercio)) {
    stop("Error: 'tipo_comercio' debe ser uno de: 'exports', 'imports'.")
  }
  
  if (!(anio %in% valid_anios)) {
    stop(paste("Error: 'anio' debe ser un año entre 2002 y", (as.integer(format(Sys.Date(), "%Y")) - 1), "."))
  }
  
  if (!(modalidad %in% valid_modalidad)) {
    stop("Error: 'modalidad' debe ser uno de: 'M', 'Y'.")
  }  
    
  page <- "files/zips"

  url <- glue::glue("{URL_BASE}/{page}/{tipo_comercio}_{anio}_{modalidad}.zip")

  return(url)
}



build_multi_years <- function(anios = NULL, tipo_comercio = NULL, modalidad = NULL){
  
  urls <- purrr::map_chr(anios, ~build_url_zip(tipo_comercio = tipo_comercio, anio = .x, modalidad = modalidad))
  
  return(urls)
  
}



# Función que descarga los archivos de las URLs - Posee progresión de descarga
descargar_archivos_zip <- function(urls, output_dir = tempdir()) {
  
  filenames <- basename(urls)
  destfiles <- glue::glue("{tempdir()}/{filenames}")
  
  response <- curl::multi_download(urls 
                             ,destfiles = destfiles 
                             ,progress = TRUE 
  )
  
  return(destfiles)
  
}


unzip_wrapper_indec_comex <- function(destfile, tipo_comercio, modalidad, priorizar, exdir = tempdir()){
  
  df_zip <- unzip(destfile, list=T)
  
  anio <- stringr::str_extract(destfile, ".*exports_(\\d{4})_.*\\.zip$", group=1) %>% 
    as.integer(.)
  
  if(anio >= 2018){
    
    prefijo_tipo_comercio <- stringr::str_sub(tipo_comercio, start = 1, end = 4)
    
    id_modalidad = ifelse(modalidad == "M", "m", "a")
    
    id_priorizar = ifelse(priorizar == "destinos","p","n")
    
    pattern_archivo = glue::glue(".*{prefijo_tipo_comercio}{id_priorizar}{id_modalidad}.*\\.csv$")
    
    filename <- df_zip %>% 
      dplyr::filter(grepl(pattern_archivo, Name)) %>% 
      pull(Name)
    
  }else{
    
    filename <- df_zip %>% 
      arrange(-Length) %>% 
      slice(1) %>% 
      pull(Name)
    
    }
  
  
  outpath <- file.path(exdir, filename)
  
  unzip(destfile, files = filename, exdir = exdir)
  
  return(outpath)

}

# Lee todos los archivos juntos. 
compilar_archivos_csv <- function(paths, sep = ";", encoding = "ISO-8859-1"){
  
  data_list <- list()
  
  for (path in paths){
    
    filename <- basename(path)
    
    df <- read.csv(file = path, sep = sep, fileEncoding = encoding)
    
    data_list[[filename]] <- df
  }
  
  
  df_completo <- bind_rows(data_list, .id = "archivo")
  
  return(df_completo)
}


INDEC_COMEX.get_countries = function(){
  
  url <- "https://comexbe.indec.gob.ar/public-api/report/countries?current=es"
  
  response <- GET(url, config = config(ssl_verifypeer = FALSE))
  
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }else{
    print("La solicitud ha sido correcta!!!")
  }
  
  requested_url <- response$url
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  return(json_data$data)
  
}

INDEC_COMEX.list_available_years = function(){
  
  url <- "https://comexbe.indec.gob.ar/public-api/staticData"
  
  response <- GET(url, config = config(ssl_verifypeer = FALSE))
  
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }else{
    print("La solicitud ha sido correcta!!!")
  }
  
  requested_url <- response$url
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  return(json_data$years$`_id`)
  
}

INDEC_COMEX.get_data <- function(tipo_comercio = NULL, modalidad = NULL, priorizar = NULL){
  
  if( is.null(priorizar)|!( priorizar %in% c('destino','ncm') ) ){
    stop("'priorizar' debe ser 'destino' o 'ncm'")
  }
  
  anios <- INDEC_COMEX.list_available_years()
  
  urls <- build_multi_years(anios = anios, tipo_comercio = tipo_comercio, modalidad = modalidad)
  
  cat("URLs construidas: ", length(urls), "\n")
  
  destfiles <- descargar_archivos_zip(urls = urls)
  
  cat("Archivos descargados\n")
  
  outpaths <- purrr::map_chr(destfiles, ~ unzip_wrapper_indec_comex(.x,
                                                                    tipo_comercio = tipo_comercio,
                                                                    modalidad = modalidad,
                                                                    priorizar = priorizar))
  
  cat("Archivos descomprimidos\n")

  df <- compilar_archivos_csv(paths = outpaths)
  
  cat("Archivos compilados\n")
  
  return(df)
  
}

# df_raw <- INDEC_COMEX.get_data(tipo_comercio = "exports", modalidad = "M", priorizar = "ncm")


# countries <- INDEC_COMEX.get_countries()
