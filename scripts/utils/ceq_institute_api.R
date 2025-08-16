# Este es un wrapper de los métodos de la api que disponibiliza la pagina en https://commitmentoequity.org/datavisualization/comparison/


library(httr)


URL_BASE <- "https://commitmentoequity.org/datavisualization/API"



make_request <- function(url){
  print(url)
  res <- GET(
    url = url,
    add_headers(
      Accept = "application/json",
      `Accept-Encoding` = "gzip, deflate, br, zstd",
      `Accept-Language` = "es-419,es;q=0.9",
      `User-Agent` = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/139.0.0.0 Mobile Safari/537.36"
    )
  )
  stop_for_status(res)
  
  return( list(url = url, data = jsonlite::fromJSON(content(res, "text", encoding = "UTF-8"), simplifyVector = TRUE)))
  
}


CEQI.list_indicators <- function(){
  
  endpoint <- "getIndicators.php"
  
  url <- glue::glue("{URL_BASE}/{endpoint}")
  
  result <- make_request(url)
  
  return(result)
  
}


CEQI.get_data <- function(indicators){
  
  endpoint <- "getDatavalues.php"
  
  indicators_string <-  paste0(indicators, collapse = ",")
  
  url <- glue::glue("{URL_BASE}/{endpoint}?indicatorsIDS={indicators_string}")
  
  result <- make_request(url)
  
  return(result)
  
}


