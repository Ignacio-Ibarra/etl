# manual_api = "https://pip.worldbank.org/api"
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


library(httr)
library(jsonlite)

# Función auxiliar para convertir a una cadena separada por comas
vector_to_string <- function(x) {
  if (is.vector(x)) {
    return(paste(x, collapse = ","))
  } else {
    return(x)  # Ya es una string
  }
}


response_to_df <- function(url){
  response <- GET(url)
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }
  
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  return(df)
}


pip_api.get_versions <-  function(){
  url <- "https://api.worldbank.org/pip/v1/versions?format=json"
  df = response_to_df(url)
  df$release_version_date <- as.Date(df$release_version, format = "%Y%m%d")
  return(df)
}



pip_api.get_aux_table <- function(table_name = ""){
  base_url <- "https://api.worldbank.org/pip/v1/aux"
  url <- modify_url(base_url, query = list(
    table = table_name,
    long_format = FALSE,
    format = "json"
  ))
  print(url)
  df = response_to_df(url)
  return(df)
}


pip_api.get_valid_params <- function(endpoint){
  base_url <- "https://api.worldbank.org/pip/v1/valid-params"
  url <- modify_url(base_url, query = list(
              endpoint = endpoint
            ))
  print(url)
  df = response_to_df(url)
  return(df)
}


pip_api.get_data <- function(year, povline, 
                             country_code="all", 
                             fill_gaps=FALSE, 
                             welfare_tye='all', 
                             reporting_level="national",
                             ppp_version = "2017",
                             version = "20240326_2017_01_02_PROD",
                             identity = "PROD") {
  
  # Armar la URL con los argumentos
  # Ejemplo de uso de modify_url
  povline <- vector_to_string(povline)
  year <- vector_to_string(year)
  country_code <- vector_to_string(country_code)
  
  base_url <- "https://api.worldbank.org/pip/v1/pip"
  url <- modify_url(base_url, query = list(
    country = country_code,
    year = year,
    povline = povline,
    fill_gaps = fill_gaps,
    welfare_tye = welfare_tye,
    reporting_level = reporting_level
    
  ))
  
  print(url)
  # Hacer la solicitud GET
  df = response_to_df(url)
  return(list(data=df, url=url))
}



# df <- pip_api.get_data(year = 'all', povline = 2.15, country_code = 'all', fill_gaps = TRUE, reporting_level = 'all') %>% 
#   select(iso3 = country_code, pais_nombre = country_name, anio = reporting_year, reporting_level, welfare_type, ingreso_consumo_medio = mean, pob = reporting_pop, gdp = reporting_gdp )
