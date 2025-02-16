
HEADER_BASE <- c("content-type" = "application/json",
                 "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/133.0.0.0 Safari/537.36")

URL_BASE <- "https://www.fao.org"

URL_BUSQUEDAS <- file.path(URL_BASE,"/fishery/services/collection/fishery/collection/search/alias")


make_request <- function(url, params, auth_token){

  headers_data <- c("Authorization" = paste("Bearer", auth_token), HEADER_BASE)
  
  response <- GET(url, add_headers(.headers = headers_data), query = params)
  
  return(response)
  
}

get_response_json <- function(response){
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }else{
    print("La solicitud ha sido correcta!!!")
  }
  
  content <- content(response, as = "text", encoding = "UTF-8")
  json_data <- jsonlite::fromJSON(content)
  
  return(json_data)
}



FAO_FISHSTATS.get_download_link <- function(topic, token_manual, filetype = "csv"){
  
  
  param_list <- list(alias = topic, lang = "en")
  
  
  response <- make_request(url = URL_BUSQUEDAS, params = param_list, auth_token = token_manual)
  
  
  json_response <- get_response_json(response)
  
  institution <- json_response$document$coverPage$author
  
  serie <- json_response$document$coverPage$series
  
  title <- json_response$document$title
  
  resources <- json_response$document$resources
  
  link <- resources %>% 
    dplyr::filter(type == filetype) %>% 
    pull(text) %>% 
    str_extract(., 'href="([^"]+)"', group=1) %>% 
    paste0(URL_BASE, ., collapse = "")
  
  result <- list(link = link, 
                 institution = institution, 
                 serie = serie,
                 title = title)
  return(result)
  
}






