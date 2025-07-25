# Base URL de la API
base_url <- "https://www.imf.org/external/datamapper/api/v1"

# Función interna para GET con headers correctos
imf_get <- function(endpoint, params = list()) {
  
  url <- file.path(base_url, endpoint) %>% 
    httr::modify_url(., query = params)
  
  # Unir parámetros en la URL en el orden recomendado
  url <- paste0(url, paste(names(params), unlist(params), sep = "=", collapse = "&"))
  
  print(url)
  res <- GET(
    url = url,
    add_headers(
      Accept = "application/json",
      `Accept-Encoding` = "identity",
      `Accept-Language` = "*"
    )
  )
  stop_for_status(res)
  
  result = list(url = url, data = jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = TRUE))
  return(result)
}

# 🔎 Endpoints básicos
IMF_DATAMAPPER.get_indicators <- function() {
  imf_get(endpoint = "indicators")$data
}

IMF_DATAMAPPER.get_countries <- function() {
  imf_get(endpoint = "countries")$data
}

IMF_DATAMAPPER.get_regions <- function() {
  imf_get(endpoint = "regions")$data
}

IMF_DATAMAPPER.get_groups <- function() {
  imf_get(endpoint = "groups")$data
}

#  Series de tiempo

IMF_DATAMAPPER.get_data <- function(indicator_id){
  response = imf_get(endpoint = indicator_id)
  
  url = response$url
  
  data = response$data$values[[indicator_id]] %>% 
    bind_rows(., .id = 'iso3') %>% 
    pivot_longer(., !iso3, names_to = 'anio', names_transform = as.integer, values_to = indicator_id)
  
  result = list(url = url, data = data)
  
  return(result)

}
