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
  
  # Filtra los links que contienen la URL base
  url <- paste0(url_base, "/", links[grepl(pattern, links)])
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!is.character(url) || length(url) != 1 || nchar(url) == 0) {
    stop("Error: 'url' debe ser una cadena de caracteres no vacía y de longitud 1.")
  }
  
  # Si pasa la verificación, devuelve el valor
  return(url)
}

# A la fecha 2024-10-17 el output que generea es este
# INDEC.cuentas_nacionales.extraer_links(id = 49, pattern = ".*serie_cgi.*\\.xls")
# [1] "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_07_24.xls"


