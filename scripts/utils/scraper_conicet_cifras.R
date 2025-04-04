library(rvest)
library(dplyr)
library(tidyr)



# Función para extraer la URL del dataset
extraer_url_dataset <- function(link_pagina) {
  if (is.na(link_pagina)) {
    return(NA_character_)
  }
  
  pagina <- tryCatch(read_html(link_pagina), error = function(e) NULL)
  
  if (is.null(pagina)) {
    return(NA_character_)
  }
  
  dataset_link <- pagina %>% 
    html_nodes("a") %>% 
    keep(~ str_detect(html_text(.x, trim = TRUE), regex("Descargar Dataset", ignore_case = TRUE))) %>%
    html_attr("href")
  
  if (length(dataset_link) > 0) {
    return(paste0("https://cifras.conicet.gov.ar", dataset_link[1]))
  } else {
    return(NA_character_)
  }
}



CONICET_CIFRAS.listar_paginas <- function() {

  url <- "https://cifras.conicet.gov.ar/publica/indice"
  
  
  pagina <- read_html(url)
  
  # Extraer la lista con id 'cifras-indice'
  lista <- pagina %>% html_node("#cifras-indice")
  
  # Obtener los elementos de primer nivel (li con class 'indice-categoria')
  categorias <- lista %>% html_nodes("li.indice-categoria")
  
  # Mapear sobre cada categoría (h2 y su ul anidado)
  df <- map_dfr(categorias, ~ {
    h2 <- .x %>% html_node("h2") %>% html_text(trim = TRUE)
    
    sublista <- .x %>% html_node("ul") # Sublista dentro de cada h2
    
    if (!is.null(sublista)) {
      subitems <- sublista %>% html_nodes("li") # Elementos dentro de la sublista
      
      map_dfr(subitems, ~ {
        h3 <- .x %>% html_node("h3") %>% html_text(trim = TRUE) %>% 
          str_replace_all("\\s*\\(\\d+\\)", "") %>% str_trim()
        
        subsublista <- .x %>% html_node("ul") # Sublista dentro de h3
        
        if (!is.null(subsublista)) {
          enlaces <- subsublista %>% html_nodes("li a") # Extraer enlaces
          
          if (length(enlaces) > 0) {
            map_dfr(enlaces, ~ {
              href <- .x %>% html_attr("href")
              tibble(
                h2 = h2,
                h3 = h3,
                texto = .x %>% html_text(trim = TRUE),
                link_pagina = href %>% paste0("https://cifras.conicet.gov.ar",., sep=""),
                id_dataset = as.integer(str_extract(href, "\\d+$")) # Extrae el número al final del href
              )
            })
          } else {
            tibble(h2 = h2, h3 = h3, texto = NA, link_pagina = NA, id_dataset = NA)
          }
        } else {
          tibble(h2 = h2, h3 = h3, texto = NA, link_pagina = NA, id_dataset = NA)
        }
      })
    } else {
      tibble(h2 = h2, h3 = NA, texto = NA, link_pagina = NA, id_dataset = NA)
    }
  })
  
  
  resultado <- df %>% drop_na(id_dataset) 
  
  return(resultado)
}


CONICET_CIFRAS.obtener_dataset_url <- function(dataset_id){
  
  link_pagina <- glue::glue("https://cifras.conicet.gov.ar/publica/grafico/show-publico/{dataset_id}")
  
  return(extraer_url_dataset(link_pagina))
}



# 
# resultados <- CONICET_CIFRAS.listar_paginas()
# 
# 
# url <- CONICET_CIFRAS.obtener_dataset_url(dataset_id = 866)


