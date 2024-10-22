# Wrapper de Indicadores del Geoportal de INDEC API
# Documentación https://datosestadisticos.indec.gob.ar/api/v1/

# El uso consite en revisar cuál es el id de la serie que se quiere descargar
# utilizando para ello la función censo_2022.get_indicator_list



library(httr)
library(jsonlite)


URL_BASE = "https://datosestadisticos.indec.gob.ar"



get_response_json <- function(url){
  
  response <- GET(url)
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }
  
  requested_url <- response$url
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  return(json_data)
}



json_to_df <- function(json_data){
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  return(df)
}


censo_2022.get_indicator_list <- function(){
  endpoint = "/api/v1/indicator/?format=json"
  url = paste0(URL_BASE, endpoint)
  cat(url)
  json_data <- get_response_json(url)
  result = json_to_df(json_data)
  return(result)
}




censo_2022.get_data = function(indicator_id){
  endpoint = glue::glue("/api/v1/indicator/{indicator_id}/data?format=json")
  url = paste0(URL_BASE, endpoint)
  json_data <- get_response_json(url)
  body <- json_data %>% 
    pluck("body") 
  data <- body %>% pluck("data")
  result <- list(url = url, data = data)
  return(result)
}


