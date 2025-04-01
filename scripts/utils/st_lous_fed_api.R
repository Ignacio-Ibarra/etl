# Este es un wrapper de la API de St. Louis Fed
# La documentación la podemos encontrar en https://fred.stlouisfed.org/docs/api/fred/

library(httr)
library(jsonlite)
# library(urltools)

URL_BASE <- "https://api.stlouisfed.org/fred/"


get_api_key <- function(){
  api_key <- Sys.getenv("stlouisfed_key")
  if (api_key == "") {
    stop("La clave de la API ('stlouisfed_key') no está configurada. 
        Asegúrate de que esté presente en tu archivo .Renviron.")
  }
  return(api_key)
}


# Define los parámetros fijos de forma global
global_params <- list(
  "api_key" =  get_api_key(),
  "file_type" = "json"
)


# Función para armar la URL con endpoint y parámetros adicionales
build_url <- function(base_url, endpoint, params) {
  
  full_url <- file.path(base_url, endpoint)
  
  # Combina parámetros fijos con adicionales
  all_params <- c(params, global_params)
  
  # Arma la URL completa con los parámetros
  url <- httr::modify_url(full_url, query = all_params)
  
  return(url)
}

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


get_result <- function(url){
  
  json_data <- get_response_json(url)
  
  result <- list(url = url, data = json_data)
  
  return(result)
  
}

json_to_df <- function(json_data){
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  return(df)
}





FRED.get_category <- function(category_id){
  endpoint <- "category"
  params <- list(
    'category_id' = category_id
  )
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
}


FRED.get_child_categories <- function(category_id){
  
  endpoint <- "category/children"
  params <- list(
    'category_id' = category_id
  )
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
  
}


FRED.get_category_series <- function(category_id){
  
  endpoint <- "category/series"
  params <- list(
    'category_id' = category_id
  )
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
  
}

FRED.get_series_metadata <- function(series_id){
  
  endpoint <- "series"
  params <- list(
    'series_id' = series_id
  )
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params)
  
  print(url)
  
  result <- get_result(url)
  
  return(result)
  
}


FRED.get_series_observations <- function(series_id, ...) {
    
  # Crea una lista de parámetros adicionales a partir de `...`
  additional_params <- list(...)
  
  allowed_params <- c(
    "realtime_start", "realtime_end", "limit", "offset", "sort_order", 
    "observation_start", "observation_end", "units", "frequency", 
    "aggregation_method", "output_type", "vintage_dates"
  )
  
  # Verifica que los parámetros adicionales estén en la lista de permitidos
  invalid_params <- setdiff(names(additional_params), allowed_params)
  if (length(invalid_params) > 0) {
    stop("Parámetros no válidos: ", paste(invalid_params, collapse = ", "))
  }
  
  # Define el endpoint y los parámetros base
  endpoint <- "series/observations"
  
  params <- c(list('series_id' = series_id), additional_params)
  
  url <- build_url(base_url = URL_BASE, endpoint = endpoint, params)

  print(url)
  
  result <- get_result(url)
  
  return(result)
}


# Ejemplo de uso.

# cpiaucsl_metadata <- FRED.get_series_metadata('CPIAUCSL')
# cpiaucsl_result <- FRED.get_series_observations('CPIAUCSL')



