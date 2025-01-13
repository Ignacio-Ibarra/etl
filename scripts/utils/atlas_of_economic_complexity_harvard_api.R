##############################
# Para usar este codigo es necesario utilizar primero el explorador de datos de Atlas. 
# Navegando hasta donde sea necesario para visualizar los datos. 
# Una vez que se llegó a la visualización deseada, ahí en la inspección del código de la web
# se pueden visualizar las request que se hacen a la api mediante payloads de GraphQL. 
# Viendo los payloads se puede observar como son las variables y cual es la query que hay que realizar. 
# 

# ejemplo: https://atlas.hks.harvard.edu/explore/overtime?productClass=SITC&product=product-SITC-656&endYear=2021&view=markets&exporter=group-1&locationLevel=country&layout=share&ordering=totals

# query <- "query GCPY2($productClass: ProductClass!, $productLevel: Int, $groupId: Int!, $productId: Int, $yearMin: Int, $yearMax: Int) {
#     data: groupCountryProductYear(
#       productClass: $productClass
#       productLevel: $productLevel
#       groupId: $groupId
#       productId: $productId
#       yearMin: $yearMin
#       yearMax: $yearMax
#     ) {
#       groupId
#       locationLevel
#       partnerCountryId
#       partnerLevel
#       productId
#       productLevel
#       year
#       exportValue
#       importValue
#       __typename
#     }
#   }"

# operation_name <- "GCPY2"
# variables <- list(
#   productClass = "SITC",
#   yearMin = 1962,
#   yearMax = 2022,
#   productId = 656,
#   groupId = 1
# )

# data_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_data_df(variable_list = variables,
#                                                     operation_name = operation_name, query = query)


# countries_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_countries_df()



# sitc2_products_4d_df <- ATLAS_OF_ECONOMIC_COMPLEXITY.get_stic2_4d_df()


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



ATLAS_OF_ECONOMIC_COMPLEXITY.get_countries_df <- function(){
  
  # Payload del request
  payload <- list(
    operationName = "GetCountries",
    variables = list(),
    query = "query GetCountries {\n  countries: locationCountry {\n    countryId\n    locationLevel\n    iso3Code\n    nameEn\n    nameShortEn\n    legacyCountryId\n    inCp\n    inMv\n    isTrusted\n    reportedServ\n    reportedServRecent\n    __typename\n  }\n}"
  )
  
  
  return(make_request(payload = payload)$data$countries %>% bind_rows())
  
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.get_stic2_4d_df <- function(){
  
  payload <- list(
    operationName = "GetSITCProducts",
    variables = list(
      servicesClass = "unilateral"
    ),
    query = "fragment ProductMetadata on Product {\n  productId\n  productLevel\n  code\n  nameEn\n  nameShortEn\n  showFeasibility\n  parent {\n    productId\n    productLevel\n    __typename\n  }\n  topParent {\n    productId\n    productLevel\n    __typename\n  }\n  __typename\n}\n\nquery GetSITCProducts($servicesClass: ServicesClass) {\n  section: productSitc(productLevel: 1, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  twoDigit: productSitc(productLevel: 2, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  fourDigit: productSitc(productLevel: 4, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n}"
  )
  
  
  return(make_request(payload = payload)$data$fourDigit %>% bind_rows())
  
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.get_stic2_2d_df <- function(){
  
  payload <- list(
    operationName = "GetSITCProducts",
    variables = list(
      servicesClass = "unilateral"
    ),
    query = "fragment ProductMetadata on Product {\n  productId\n  productLevel\n  code\n  nameEn\n  nameShortEn\n  showFeasibility\n  parent {\n    productId\n    productLevel\n    __typename\n  }\n  topParent {\n    productId\n    productLevel\n    __typename\n  }\n  __typename\n}\n\nquery GetSITCProducts($servicesClass: ServicesClass) {\n  section: productSitc(productLevel: 1, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  twoDigit: productSitc(productLevel: 2, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  fourDigit: productSitc(productLevel: 4, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n}"
  )
  
  
  return(make_request(payload = payload)$data$twoDigit %>% bind_rows())
  
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.get_stic2_section_df <- function(){
  
  payload <- list(
    operationName = "GetSITCProducts",
    variables = list(
      servicesClass = "unilateral"
    ),
    query = "fragment ProductMetadata on Product {\n  productId\n  productLevel\n  code\n  nameEn\n  nameShortEn\n  showFeasibility\n  parent {\n    productId\n    productLevel\n    __typename\n  }\n  topParent {\n    productId\n    productLevel\n    __typename\n  }\n  __typename\n}\n\nquery GetSITCProducts($servicesClass: ServicesClass) {\n  section: productSitc(productLevel: 1, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  twoDigit: productSitc(productLevel: 2, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n  fourDigit: productSitc(productLevel: 4, servicesClass: $servicesClass) {\n    ...ProductMetadata\n    __typename\n  }\n}"
  )
  
  
  return(make_request(payload = payload)$data$section %>% bind_rows())
  
  
}



ATLAS_OF_ECONOMIC_COMPLEXITY.get_data_df <- function(variable_list, operation_name, query){
  
  
  # Crear el payload
  payload <- crear_payload(operation_name, variable_list, query)
  
  # Hacer la petición
  return(make_request(payload = payload)$data$data %>% bind_rows())
}








