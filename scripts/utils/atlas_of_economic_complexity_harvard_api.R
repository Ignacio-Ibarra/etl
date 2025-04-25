##############################
# ATLAS_OF_ECONOMIC_COMPLEXITY.list_datasets() se obtiene la lista de todos los datasets disponibles. 
# ATLAS_OF_ECONOMIC_COMPLEXITY.download_by_id() permite descargar un archivo desde dataverse con el id correspondiente,
# el mismo se obtiene utilizando la función anterior. 
###############################

library(httr)
library(jsonlite)

# URL del endpoint
url_base <- "https://atlas.hks.harvard.edu/api/graphql"


# Cabeceras necesarias
headers <- c(
  "Content-Type" = "application/json",
  "Accept" = "*/*",
  "Origin" = "https://atlas.hks.harvard.edu",
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36"
)


# Función para construir el payload
crear_payload <- function(operation_name, variables, query) {
  # Construir el payload como una lista
  payload <- list(
    operationName = operation_name,
    variables = variables,
    query = query
  )
  
  # Convertir el payload a formato JSON
  return(payload)
}





make_request <- function(payload){
  
  # Hacer la solicitud POST
  response <- POST(
    url = url_base,
    body = toJSON(payload, auto_unbox = TRUE),
    encode = "json",
    add_headers(.headers = headers)
  )
  
  # Verificar el estado de la respuesta
  if (status_code(response) == 200) {
    # Parsear el contenido de la respuesta
    data <- content(response, as = "parsed", type = "application/json")
    return(data)
  } else {
    # Imprimir error
    cat("Error en la petición:", status_code(response), "\n")
    print(content(response, as = "text", encoding = "UTF-8"))
  }
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.list_datasets <- function(){
  
  
  # Payload del request
  payload <- list(
    operationName = "GetDownloadsData",
    variables = list(),
    query = "query GetDownloadsData {
  downloadsTable {
    tableId
    tableName
    tableDataType
    repo
    complexityData
    productLevel
    facet
    yearMin
    yearMax
    displayName
    productClassificationHs92
    productClassificationHs12
    productClassificationSitc
    productClassificationServicesUnilateral
    dvFileId
    dvFileName
    dvFileSize
    dvPublicationDate
    doi
    columns {
      columnId
      columnName
      complexity
      description
      decimalPlaces
      dataType
      notes
      __typename
    }
    __typename
  }
}"
  )
  
  
  return(make_request(payload = payload)$data$downloadsTable %>% 
           purrr::map_dfr(., ~within(.x, rm(columns))))
  
}




ATLAS_OF_ECONOMIC_COMPLEXITY.get_countries_df <- function(){
  
  # Payload del request
  payload <- list(
    operationName = "GetCountries",
    variables = list(),
    query = "query GetCountries {\n  countries: locationCountry {\n    countryId\n    locationLevel\n    iso3Code\n    nameEn\n    nameShortEn\n    legacyCountryId\n    inCp\n    inMv\n    isTrusted\n    reportedServ\n    reportedServRecent\n    __typename\n  }\n}"
  )
  
  
  return(make_request(payload = payload)$data$countries %>% bind_rows())
  
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.download_by_id <- function(dvFieldId, destfile, timeout=NULL){
  
  url <- glue::glue("https://dataverse.harvard.edu/api/access/datafile/{dvFieldId}")
  
  if(is.null(timeout)){timeout = 60}
  h <- curl::new_handle(timeout = timeout)
  curl::curl_download(url, destfile, handle = h, quiet = FALSE)
  
  return(list(url = url, destfile = destfile))
  
}
  
  
