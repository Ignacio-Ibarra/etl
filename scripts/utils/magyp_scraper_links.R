require(httr)
library(urltools)

corregir_url <- function(url) {
  
  # Separa la URL en sus componentes para trabajar con ella
  componentes <- url_parse(url)
  
  # Codifica caracteres especiales solo en el path
  componentes$path <- gsub(" ", "%20", componentes$path)  # Codifica espacios
  componentes$path <- URLencode(componentes$path, reserved = TRUE)
  
  # Reconstruye la URL a partir de sus componentes
  url_corregida <- url_compose(componentes)
  
  return(url_corregida)
}


MAGYP.extraer_links_datos_abiertos <- function(page_suffix, h3_target ){
  
  url_base <- "https://datos.magyp.gob.ar/dataset/"
  
  page_url <- paste0(url_base,page_suffix)
  
  response <- GET(page_url, 
                  config = config(ssl_verifypeer = FALSE),
                  add_headers(
                    `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/114.0.0.0 Safari/537.36",
                    `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
                    `Accept-Language` = "en-US,en;q=0.5",
                    `Accept-Encoding` = "gzip, deflate, br",
                    `Connection` = "keep-alive",
                    `Upgrade-Insecure-Requests` = "1",
                    `Sec-Fetch-Dest` = "document",
                    `Sec-Fetch-Mode` = "navigate",
                    `Sec-Fetch-Site` = "none",
                    `Sec-Fetch-User` = "?1"
                  )
                )
  
  # Verificar el contenido de la respuesta
  web_content <- httr::content(response, "text") %>% 
    rvest::read_html(.)
  
  # Extraer todos los div con clase "pkg-container"
  pkg_containers <- web_content %>%
    html_nodes(".pkg-container")
  
  # Extraer los enlaces "DESCARGAR" y sus respectivos h3
  links_df <- pkg_containers %>%
    # Iterar sobre cada div "pkg-container"
    purrr::map_df(~ {
      # Extraer los enlaces "DESCARGAR" dentro del div.pkg-actions
      href <- .x %>%
        html_nodes(".pkg-actions a") %>%
        purrr::keep(~ html_text(.x, trim = TRUE) == "DESCARGAR") %>%
        html_attr("href")
      
      # Extraer el título h3 dentro del div.package-info
      h3_text <- .x %>%
        html_nodes(".package-info h3") %>%
        html_text(trim = TRUE)
      
      paragraph_text <- .x %>%
        html_nodes(".package-info p") %>%
        html_text(trim = TRUE)
      
      # Devolver los valores en un data.frame
      tibble(
        href = href,
        h3 = h3_text,
        paragraph = paragraph_text
        )
    })
  
  filtered_links <- links_df %>% 
    dplyr::filter(h3 == h3_target)
  
  # Verificar que 'url' sea un vector de longitud 1 y no vacío
  if (!(nrow(filtered_links) == 1)) {
    stop("Error: Verificar los parámetros 'page_suffix' o 'h3_target' no se han pasado correctamente")
  }
  
  # Devuelve una lista con la URL y el texto asociado
  result <- list(url = filtered_links$href, title = filtered_links$h3, text = filtered_links$paragraph)
  
  return(result)
}



MAGYP.extraer_links_informes_bovinos <- function(pattern){
  
  # URL de la página que quieres scrapear
  base_url <- "https://www.magyp.gob.ar/sitio/areas/bovinos/informacion_interes/informes"
  
  
  url <- paste0(base_url, "/index.php")
  
  web_content <- rvest::read_html(url)
  
    # Extraer los nodos <a> con href que coincidan con el patrón
  target_a_nodes <- web_content %>% 
    html_nodes('a') %>% 
    keep(~ grepl(pattern, html_attr(., "href"), ignore.case = TRUE))
  
  planillas <- target_a_nodes %>% 
    map_df(~ {
      href <- html_attr(., "href")
      texto <- html_text(.)
      data.frame(
        texto = texto,
        link = file.path(base_url, href) %>% corregir_url(.),
        stringsAsFactors = FALSE
      )
    }) %>% 
    dplyr::filter(texto!="")
  
  if (nrow(planillas) == 0){
    stop("No se han encontrado planillas de cálculo para descargar en la página")
  }
  
  return(planillas)
  
}
