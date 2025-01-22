# Este es un wrapper de la API del FMI. 
# La documetanción para utilizar el JSON RESTful Web Service 
# se puede encontrar aquí https://www.bcra.gob.ar/Catalogo/Content/files/pdf/principales-variables-v3.pdf

library(httr)
library(jsonlite)

URL_BASE <- "https://api.bcra.gob.ar"



# Función para armar la URL con endpoint y parámetros adicionales
build_url <- function(base_url, endpoint, params = NULL) {
  
  full_url <- file.path(base_url, endpoint)
  
  # Arma la URL completa con los parámetros
  url <- ifelse(!is.null(params), httr::modify_url(full_url, query = params), full_url)
  
  return(url)
}

get_response_json <- function(url){
  
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
  
  return(json_data)
}


get_result <- function(url){
  
  json_data <- get_response_json(url)
  
  result <- list(url = url, data = json_data$results)
  
  return(result)
  
}


BCRA.estadisticas_monetarias.listar_estadisticas <- function(){
  endpoint <- "estadisticas/v3.0/Monetarias"
  
  url <- build_url(base_url = URL_BASE, endpoint = endpoint)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
}



BCRA.estadisticas_monetarias.get_data <- function(idVariable, 
                                                  desde = NULL, hasta = NULL, 
                                                  offset = NULL, limit = NULL) {
  
  # Validación básica
  if (missing(idVariable)) {
    stop("El parámetro 'idVariable' es obligatorio.")
  }
  
  # Crear lista de parámetros
  params <- list(
    desde = if (!is.null(desde)) as.character(desde) else NULL,
    hasta = if (!is.null(hasta)) as.character(hasta) else NULL,
    offset = if (!is.null(offset)) as.character(offset) else NULL,
    limit = if (!is.null(limit)) as.character(limit) else NULL
  )
  
  # Filtrar los valores NULL para no incluirlos en la URL
  params <- params[!sapply(params, is.null)]
  
  endpoint <- glue::glue("estadisticas/v3.0/Monetarias/{idVariable}")
  
  # Construir la URL utilizando build_url
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params = params)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
}


