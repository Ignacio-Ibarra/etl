# Wrapper de UNSTATS Analisys of Main Aggregates API
# Documentación https://unstats.un.org/unsd/amaapi/swagger/index.html
# endpoints: https://unstats.un.org/unsd/amaapi/swagger/v1/swagger.json

# El uso consite en revisar cuál es el id de la serie que se quiere descargar
# utilizando para ello la función ama_api.get_available_series,
# los códigos de paises que se quieren descargar ama_api.get_countries
# y los 


rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


library(httr)
library(jsonlite)


URL_BASE = "https://unstats.un.org/unsd/amaapi"



get_response_result <- function(url){
  response <- GET(url)
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }
  
  requested_url <- response$url
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  result <- list(url = requested_url, data = df)
  return(result)
}


post_response_result <- function(url, body){
  # Realizar la solicitud POST
  response <- POST(url, 
                   add_headers(
                     'accept' = 'text/plain',
                     'Content-Type' = 'application/json-patch+json'
                   ),
                   body = body,
                   encode = "json")
  
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }
  
  # Parsear el contenido a JSON y convertirlo a data frame
  requested_url <- response$url
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  result <- list(url = requested_url, data = df)
  return(result)
}


ama_api.get_countries <- function(){
  endpoint = "/api/Country"
  url = paste0(URL_BASE, endpoint)
  result = get_response_result(url)
  return(result)
}

# countries_df <- ama_api.get_countries()


ama_api.get_available_files = function(){
  endpoint = "/api/File"
  url = paste0(URL_BASE, endpoint)
  result = get_response_result(url)
  return(result)
}


# files_df <- ama_api.get_available_files()


ama_api.get_available_series = function(){
  endpoint = "/api/Series"
  url = paste0(URL_BASE, endpoint)
  result = get_response_result(url)
  return(result)
}


# series_df <- ama_api.get_available_series()


ama_api.get_full_glossary = function(){
  endpoint = "/api/Metadata/glossary/0"
  url = paste0(URL_BASE, endpoint)
  result = get_response_result(url)
  return(result)
}


# glossary_df <- ama_api.get_full_glossary()


ama_api.get_data = function(serieId, m49_codes, years){
  endpoint = "/api/Data/basic/"
  url = paste0(URL_BASE, endpoint, serieId)
  # Crear el cuerpo de la solicitud
  body <- toJSON(list(
    paramCodes = c(m49_codes), # esto es por si me pasan un entero y no un vector de varios enteros
    years = c(years) # esto es por si me pasan un entero y no un vector de varios enteros
  ), auto_unbox = TRUE)
  
  result = post_response_result(url = url, body=body)
  return(result)
}


# data = ama_api.get_data(serieId = 26, m49_codes = c(32,76), years = 1900:2024)
