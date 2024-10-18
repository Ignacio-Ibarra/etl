library(rvest)
library(httr)


BEL.extraer_links = function(page_url, url_base){
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href")
  
  # Filtra los links que contienen la URL base
  url <- links[grepl(url_base, links)] %>% str_extract(., "http.*")
  
  return(url)
}



