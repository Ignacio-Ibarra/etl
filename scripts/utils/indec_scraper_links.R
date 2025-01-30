library(rvest)
library(httr)


# ids <- c(
#   "Agregado macroeconómicos (PIB)" = 47,
#   "Generación del ingreso" = 49,
#   )



# id <- 49 # con esto busco en la página de Cuenta de Generación del Ingreso
# pattern <- ".*serie_cgi.*\\.xls"  # con esto busco el link que contiene un archivo con un patrón específico. 

INDEC.cuentas_nacionales.extraer_links = function(id, pattern){
  
  url_base <- "https://www.indec.gob.ar"
  
  page_url <- paste0(url_base,"/Nivel4/Tema/3/9/",id)
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract(., pattern = "ftp/.*")
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(pattern, links)]
  filtered_texts <- text_links[grepl(pattern, links)]
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!is.character(filtered_links) || length(filtered_links) != 1 || nchar(filtered_links) == 0) {
    stop("Error: 'url' debe ser una cadena de caracteres no vacía y de longitud 1.")
  }
  
  
  # Construye la URL completa
  url <- paste0(url_base, "/", filtered_links)
  
  # Devuelve una lista con la URL y el texto asociado
  result <- list(url = url, text = filtered_texts)
  
  return(result)
}

# A la fecha 2024-10-17 el output que generea es este
# INDEC.cuentas_nacionales.extraer_links(id = 49, pattern = ".*serie_cgi.*\\.xls")
# [1] "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_07_24.xls"




INDEC.poblacion_proyecciones.extraer_links = function(id, pattern, all=FALSE){
  
  url_base <- "https://www.indec.gob.ar"
  
  page_url <- paste0(url_base,"/Nivel4/Tema/2/24/",id)
  
  # Obtener el contenido de la página
  response <- GET(page_url)
  html_content <- content(response, as = "text", encoding = "UTF-8")
  
   # Parsear el contenido HTML
  page <- read_html(html_content)
  
  xpath_query <- '//*[@id="contenidoPrincipal"]/div/div[1] | //*[@id="contenidoPrincipal"]/div/div[2]/div/div[1]/div/div | //*[@id="10"]/div[1]'
  
  titulo <- page %>% 
    html_nodes(xpath = xpath_query) %>% 
    html_text(., trim = TRUE) %>% paste0(., collapse = ". ")
  
  # Extraer los links y sus textos acompañantes
  links_info <- page %>%
    html_nodes(xpath ='//*[@id="contenidoPrincipal"]') %>% 
    html_elements("a.a-color2") %>%
    purrr::map_df(~ data.frame(
      provincia = html_text(.x, trim = TRUE),
      link = html_attr(.x, "href"),
      stringsAsFactors = FALSE
    )) %>% 
    mutate(link = paste0(url_base, link),
           titulo = titulo)
  
  return(links_info)
  
}



INDEC.balanza_pagos.extraer_links = function(id, pattern){
  
  url_base <- "https://www.indec.gob.ar"
  
  page_url <- paste0(url_base,"/Nivel4/Tema/3/35/",id)
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract(., pattern = "ftp/.*")
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(pattern, links)]
  filtered_texts <- text_links[grepl(pattern, links)]
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!is.character(filtered_links) || length(filtered_links) != 1 || nchar(filtered_links) == 0) {
    stop("Error: 'url' debe ser una cadena de caracteres no vacía y de longitud 1.")
  }
  
  
  # Construye la URL completa
  url <- paste0(url_base, "/", filtered_links)
  
  # Devuelve una lista con la URL y el texto asociado
  result <- list(url = url, text = filtered_texts)
  
  return(result)
}




INDEC.intercambio_comercial_argentino.extraer_links = function(id, pattern){
  
  url_base <- "https://www.indec.gob.ar"
  
  page_url <- paste0(url_base,"/Nivel4/Tema/3/2/",id)
  
  # Obtiene el contenido de la página web
  web_content <- read_html(page_url)
  
  # Obtiene los href y el texto de cada enlace
  links <- web_content %>%
    html_nodes("a") %>%
    html_attr("href")
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  filtered_links <- links[grepl(pattern, links)]
  filtered_texts <- text_links[grepl(pattern, links)]
  
    # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!is.character(filtered_links) || length(filtered_links) != 1 || nchar(filtered_links) == 0) {
    stop("Error: 'url' debe ser una cadena de caracteres no vacía y de longitud 1.")
  }
  
  
  # Construye la URL completa
  url <- paste0(url_base, "/", filtered_links)
  
  # Devuelve una lista con la URL y el texto asociado
  result <- list(url = url, text = filtered_texts)
  
  # Si pasa la verificación, devuelve el valor
  return(result)
}



