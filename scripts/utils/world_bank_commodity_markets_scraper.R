library(rvest)
library(httr)

# pattern = ".*CMO-Historical-Data-Monthly\\.xlsx"

WorldBank.CommodityMarkets.extraer_links = function(pattern){
  
  page_url <- "https://www.worldbank.org/en/research/commodity-markets"
  
    # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Busco los links
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") 
    
  # Filtra los links que contienen la URL base
  url <- links %>% 
    keep(., grepl(pattern, .))
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    stop("Error: 'url' debe ser una cadena de caracteres no vacía y de longitud 1.")
  }
  
  # Si pasa la verificación, devuelve el valor
  return(url)
}
