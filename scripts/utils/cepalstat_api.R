library(httr)
library(jsonlite)

base_url <- "https://api-cepalstat.cepal.org/cepalstat/api/v1"

# Función base para construir requests
make_request <- function(endpoint, query = list(), path_params = list()) {
  url <- base_url
  for (param in path_params) {
    url <- paste0(url, "/", param)
  }
  url <- paste0(url, endpoint)
  
  response <- GET(url, query = query)
  stop_for_status(response)
  content(response, as = "parsed", simplifyVector = TRUE)
}

# Árbol temático
cepalstat_api.get_thematic_tree <- function(lang = "es", format = "json") {
  make_request("/thematic-tree", query = list(lang = lang, format = format))
}

# Data del indicador
cepalstat_api.get_indicator_data <- function(indicator_id, lang = "es", format = "json", members = NULL) {
  query <- list(lang = lang, format = format)
  if (!is.null(members)) query$members <- members
  make_request(paste0("/indicator/", indicator_id, "/records"), query)
}

# Metadata
cepalstat_api.get_indicator_metadata <- function(indicator_id, lang = "es", format = "json") {
  make_request(paste0("/indicator/", indicator_id, "/metadata"), query = list(lang = lang, format = format))
}

# Dimensiones
cepalstat_api.get_indicator_dimensions <- function(indicator_id, lang = "es", format = "json", in_present = 1, path = 0) {
  make_request(paste0("/indicator/", indicator_id, "/dimensions"), query = list(
    lang = lang, format = format, `in` = in_present, path = path
  ))
}

# Notas
cepalstat_api.get_indicator_footnotes <- function(indicator_id, lang = "es", format = "json") {
  make_request(paste0("/indicator/", indicator_id, "/footnotes"), query = list(lang = lang, format = format))
}

# Fuentes
cepalstat_api.get_indicator_sources <- function(indicator_id, lang = "es", format = "json") {
  make_request(paste0("/indicator/", indicator_id, "/sources"), query = list(lang = lang, format = format))
}

# Publicaciones
cepalstat_api.get_indicator_publications <- function(indicator_id, lang = "es", format = "json") {
  make_request(paste0("/indicator/", indicator_id, "/publications"), query = list(lang = lang, format = format))
}

# Datos completos (cubos)
cepalstat_api.get_indicator_cube <- function(indicator_id, lang = "es", format = "json", members = NULL, in_present = 1, path = 0) {
  query <- list(lang = lang, format = format, `in` = in_present, path = path)
  if (!is.null(members)) query$members <- members
  make_request(paste0("/indicator/", indicator_id, "/data"), query)
}



