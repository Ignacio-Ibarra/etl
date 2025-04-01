# Documentación https://apiportal.wto.org/api-details#api=version1&operation=get-data-i-i


library(httr)
library(jsonlite)


URL_BASE <- "https://api.wto.org/timeseries/v1"


API_KEY = Sys.getenv('WTO_API_PRIMARY_KEY')


# Configuración de los encabezados
HEADERS <- add_headers(
  `Content-Type` = "application/json",
  `Cache-Control` = "no-cache",
  `Ocp-Apim-Subscription-Key` = API_KEY
)


get_request <- function(endpoint, body){
  
  url <- file.path(URL_BASE, endpoint)
  
  # Convertir el cuerpo a formato JSON
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  
  # Realizar la solicitud POST
  response <- GET(url, HEADERS, body = body_json)
  
  
  # Verificar el estado de la respuesta
  if (status_code(response) == 200){
    return(response)
  }else {
    # Mostrar mensaje de error
    parsed_error = fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    str_error = glue::glue("Status Code: {parsed_error$statusCode} - '{parsed_error$message}'")
    stop(str_error)
    
  }
  
}


post_request <- function(endpoint, body){
  
  url <- file.path(URL_BASE, endpoint)
  
  # Convertir el cuerpo a formato JSON
  body_json <- toJSON(body, auto_unbox = TRUE)
  
  
  # Realizar la solicitud POST
  response <- POST(url, HEADERS, body = body_json)
  
  
  # Verificar el estado de la respuesta
  if (status_code(response) == 200){
    return(response)
  }else {
    # Mostrar mensaje de error
    parsed_error = fromJSON(content(response, as = "text", encoding = "UTF-8"), flatten = TRUE)
    str_error = glue::glue("Status Code: {parsed_error$statusCode} - '{parsed_error$message}'")
    stop(str_error)
    
  }
}

response_to_data <- function(response){

  raw_content <- content(response, as = "raw")
  data_text <- rawToChar(raw_content)
  
  parsed_data <- fromJSON(data_text, flatten = TRUE)

  return(parsed_data)
}


wto_timeseries_api.get_years <- function(lang = 1){
  
  
  full_url <- file.path(URL_BASE,'years')
  
  # Hacer la solicitud GET utilizando la función ya definida
  response <- httr::GET(full_url, HEADERS)
  
  # Convertir la respuesta a un formato legible
  parsed_data <- response_to_data(response)
  
  # Retornar los datos obtenidos
  return(parsed_data)
}


wto_timeseries_api.get_reporting_economies <- function(name = NULL, ig = NULL, reg = NULL, gp = NULL, lang = 1) {
  # Definir el endpoint específico para "reporters"
  endpoint <- "reporters"
  
  # Crear el cuerpo de la solicitud con los parámetros
  body <- list(
    name = name,
    ig = ig,
    reg = reg,
    gp = gp,
    lang = lang
  )
  
  # Filtrar los parámetros nulos para no enviarlos en el cuerpo de la solicitud
  body <- Filter(Negate(is.null), body)
  
  # Hacer la solicitud GET utilizando la función ya definida
  response <- get_request(endpoint, body)
  
  # Convertir la respuesta a un formato legible
  parsed_data <- response_to_data(response)
  
  # Retornar los datos obtenidos
  return(parsed_data)
}



wto_timeseries_api.get_products <- function(name = NULL, product_sector = NULL, lang = 1) {
  
  endpoint <- "products"
  
  # Crear el cuerpo de la solicitud con los parámetros
  body <- list(
    name = name,
    pc = product_sector,
    lang = lang
  )
  
  # Filtrar los parámetros nulos para no enviarlos en el cuerpo de la solicitud
  body <- Filter(Negate(is.null), body)
  
  # Hacer la solicitud GET utilizando la función ya definida
  response <- get_request(endpoint, body)
  
  # Convertir la respuesta a un formato legible
  parsed_data <- response_to_data(response)
  
  # Retornar los datos obtenidos
  return(parsed_data)
  
}


wto_timeseries_api.get_data_points <- function(indicator_code, 
                                               reporting_economies = NULL, 
                                               partner_economies = NULL, 
                                               time_period = NULL, 
                                               product_sectors = 'all', 
                                               include_sub_products = FALSE, 
                                               output_format = 'json', 
                                               output_mode = 'full', 
                                               decimal_places = NULL, 
                                               offset = NULL, 
                                               max_records = 999999, 
                                               heading_style = NULL, 
                                               language_id = 1, 
                                               include_metadata = FALSE) {

  endpoint <- 'data'
  
  
  # Crear lista de parámetros excluyendo valores NULL
  query_params <- list(
    i = indicator_code, 
    r = reporting_economies, 
    p = partner_economies, 
    ps = time_period, 
    pc = product_sectors, 
    spc = include_sub_products, 
    fmt = output_format, 
    mode = output_mode, 
    dec = decimal_places, 
    off = offset, 
    max = max_records, 
    head = heading_style, 
    lang = language_id, 
    meta = include_metadata
  )

  
  # Filtrar los parámetros nulos para no enviarlos en el cuerpo de la solicitud
  query_params <- Filter(Negate(is.null), query_params)
  
  
  url <- file.path(URL_BASE,endpoint)
  
  # Construcción automática de la URL con parámetros
  full_url <- modify_url(url, query = query_params)
  
  # Hacer la solicitud GET utilizando la función ya definida
  response <- httr::GET(url = full_url, HEADERS)
  
  # Convertir la respuesta a un formato legible
  parsed_data <- response_to_data(response)
  
  # Retornar los datos obtenidos
  return(parsed_data$Dataset)

}

# rep_economies <- wto_timeseries_api.get_reporting_economies()


# data <- wto_timeseries_api.get_data_points(
#   indicator_code = "ITS_CS_AX6", 
#   time_period = '2023'
# )

