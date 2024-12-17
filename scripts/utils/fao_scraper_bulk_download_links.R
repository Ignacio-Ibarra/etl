require(httr)
require(jsonlite)


base_url_en <- "https://faostatservices.fao.org/api/v1/en"

base_url_es <- "https://faostatservices.fao.org/api/v1/es"

language_selector <- c('en' = base_url_en, 'es' = base_url_es)


get_response_json <- function(url){
  
  response <- GET(url)
  
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
  
  return(json_data)
}


get_result <- function(json_data){
  
  json_data <- get_response_json(url)
  
  result <- list(url = url, data = json_data)
  
  return(result)
  
}

json_to_df <- function(json_data){
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  return(df)
}



FAO.list_domains <- function(language = 'en'){
  
  base_url = language_selector[language]
  
  endpoint = "groupsanddomains"
  
  url = file.path(base_url, endpoint)
  
  json_data = get_response_json(url)
  
  data = json_data$data
  
  return(data)
}


FAO.get_dataset_metadata <- function(dataset_code, language = 'en'){
  
  base_url = language_selector[language]
  
  endpoint = "metadata"
  
  url = file.path(base_url, endpoint, dataset_code)
  
  json_data = get_response_json(url)
  
  data = json_data$data
  
  return(data)
}


FAO.get_dataset_docs <- function(dataset_code, language = 'en'){
  
  base_url = language_selector[language]
  
  endpoint = "documents"
  
  url = file.path(base_url, endpoint, dataset_code)
  
  json_data = get_response_json(url)
  
  data = json_data$data
  
  return(data)
}


FAO.get_bulkdownload_urls <- function(dataset_code, language = 'en'){
  
  base_url = language_selector[language]
  
  endpoint = "bulkdownloads"
  
  url = file.path(base_url, endpoint, dataset_code)
  
  json_data = get_response_json(url)
  
  data = json_data$data
  
  return(data)
}



make_wget <- function(url, destfile) {
  # Comando wget con las cabeceras necesarias
  comando <- paste(
    "wget --header=\"User-Agent: Mozilla/5.0\"",
    shQuote(url),
    "-O", shQuote(destfile) # -O indica el archivo de salida
  )
  
  # Ejecutar el comando
  resultado <- system(comando, intern = TRUE)
  
  # Verificar si el archivo se descargó correctamente
  if (file.exists(destfile)) {
    message("Archivo descargado exitosamente: ", destfile)
  } else {
    warning("No se pudo descargar el archivo.")
  }
  
}


FAO.descargar_archivo <- function(url_descarga, zip_destfile, file_to_download){
  
  
  exdir = dirname(zip_destfile)
  
  make_wget(url = url_descarga, destfile = zip_destfile)
  
  # Verificar si el archivo se descargó correctamente
  if (file.exists(zip_destfile)) {
    
    message("Archivo descargado exitosamente: ", zip_destfile)
    
    unzip(zip_destfile, files = file_to_download, exdir = exdir)
  
    } else {
    warning("No se pudo descargar el archivo.")
  }
  
}


# Ejemplo 
#
# dominios <- FAO.list_domains(language = 'es')
# 
# 
# dataset_metadata <- FAO.get_dataset_metadata("RL", language = 'es')
# 
# bulkdownload_urls <- FAO.get_bulkdownload_urls("RL", language = "en")
# 
# 
# url <- bulkdownload_urls %>% 
#   dplyr::filter(Type == "All") %>% 
#   pull(URL)
# 
# zip_filename <- 'Inputs_LandUse_E_All_Data.zip'
# 
# zip_destfile <- glue::glue("{tempdir()}/{zip_filename}")
# 
# download_filename <- "Inputs_LandUse_E_All_Data.csv"
# 
# FAO.descargar_archivo(url_descarga = url, zip_destfile = zip_destfile, file_to_download = download_filename)
# 
# land_use <- read_csv(file.path(tempdir(), "Inputs_LandUse_E_All_Data.csv"))
