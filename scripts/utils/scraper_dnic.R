library(rvest)
library(httr)



# Función que descarga todos los links del año requerido
DNIC.listar_links_descarga <- function(patron){
  
  url_consulta <- "https://www.argentina.gob.ar/ciencia/indicadorescti/datasets"
  
  # Obtiene el contenido de la página web
  web_content <- read_html(url_consulta)
  
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract(., pattern = patron)
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(patron, links)] %>% gsub("blank:#","",.)
  filtered_texts <- text_links[grepl(patron, links)]
  
  
  result <- data.frame(filtered_texts, filtered_links)
  
  return(result)
}

