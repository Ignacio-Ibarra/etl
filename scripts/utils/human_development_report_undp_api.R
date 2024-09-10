# manual_api = "https://hdr.undp.org/sites/default/files/2023-24_HDR/HDRO_data_api_manual.pdf"
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


hdr_api.get_metadata <- function(apikey, entityName){
  url <- glue::glue("https://hdrdata.org/api/Metadata/{entityName}?apikey={apikey}")
  
  print(url)
  
  # Hacer la solicitud GET
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

hdr_api.get_data <- function(apikey, country_code, year, indicator) {
  # Armar la URL con los argumentos
  # Ejemplo de uso de modify_url
  year <- vector_to_string(year)
  country_code <- vector_to_string(country_code)
  indicator <- vector_to_string(indicator)
  
  base_url <- "https://hdrdata.org/api/CompositeIndices/query"
  url <- modify_url(base_url, query = list(
    apikey = apikey,
    countryOrAggregation = country_code,
    year = year,
    indicator = indicator
  ))
  
  print(url)
  # Hacer la solicitud GET
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


countries <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                  entityName = "Countries")

hdr_regions <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                    entityName = "HDRegions")

hdr_groups <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                   entityName = "HDGroups")

hdr_indictators <- hdr_api.get_metadata(apikey = Sys.getenv("hdr_key"),
                                        entityName = "Indicators")

area_codes <- c(countries$code, hdr_regions$code, hdr_groups$code)

df <- hdr_api.get_data(apikey = Sys.getenv("hdr_key"),
                       country_code = area_codes,
                       year = 1950:2023,
                       indicator = "le")
