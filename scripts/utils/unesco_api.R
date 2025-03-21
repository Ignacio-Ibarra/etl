# Aquí se tiene el schema.json completo para armar el wrapper https://api.uis.unesco.org/api/public/openapi/schema.json
# Aquí se tiene la documentación https://api.uis.unesco.org/api/public/documentation/

library(httr)
library(jsonlite)

# Definir la URL base de la API
base_url <- "https://api.uis.unesco.org/api/public"

# Función para obtener la lista de indicadores disponibles
UNESCO.get_indicators <- function(version = NULL) {
  url <- paste0(base_url, "/definitions/indicators")
  if (!is.null(version)) {
    url <- paste0(url, "?version=", version)
  }
  
  response <- GET(url)
  if (status_code(response) == 200) {
    return(fromJSON(content(response, as = "text")))
  } else {
    stop("Error al obtener la lista de indicadores: ", status_code(response))
  }
}

# Función para obtener la lista de países disponibles
UNESCO.get_countries <- function(version = NULL) {
  url <- paste0(base_url, "/definitions/geounits")
  if (!is.null(version)) {
    url <- paste0(url, "?version=", version)
  }
  
  response <- GET(url)
  if (status_code(response) == 200) {
    return(fromJSON(content(response, as = "text")))
  } else {
    stop("Error al obtener la lista de países: ", status_code(response))
  }
}

# Función para obtener datos de un indicador y/o país con opciones adicionales
UNESCO.get_indicator_data <- function(indicators = NULL, countries = NULL, start_year = NULL, end_year = NULL, 
                               footnotes = FALSE, indicator_metadata = FALSE, version = NULL) {
  url <- paste0(base_url, "/data/indicators?")
  
  # Construcción ordenada de la URL con parámetros
  params <- list()
  if (!is.null(indicators)) params$indicator <- paste(indicators, collapse = "&indicator=")
  if (!is.null(countries)) params$geoUnit <- paste(countries, collapse = "&geoUnit=")
  if (!is.null(start_year)) params$start <- start_year
  if (!is.null(end_year)) params$end <- end_year
  if (footnotes) params$footnotes <- "true"
  if (indicator_metadata) params$indicatorMetadata <- "true"
  if (!is.null(version)) params$version <- version
  
  # Unir parámetros en la URL en el orden recomendado
  url <- paste0(url, paste(names(params), unlist(params), sep = "=", collapse = "&"))
  
  response <- GET(url, add_headers("Accept-Encoding" = "gzip"))
  if (status_code(response) == 200) {
    
    resultado = list(url_consulta = url, response = fromJSON(content(response, as = "text")))
    return(resultado)
  } else {
    stop("Error al obtener los datos: ", status_code(response))
  }
}


# # Ejemplo de uso
# indicadores <- get_indicators()
# paises <- get_countries()
# datos <- get_indicator_data(indicators = c("EXPGDP.TOT"), indicator_metadata = TRUE) # Si quiero GERD as a percentage of GDP y si quiero metadatos 

