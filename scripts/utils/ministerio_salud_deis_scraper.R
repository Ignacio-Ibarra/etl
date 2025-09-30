library(httr)
library(rvest)
library(dplyr)
library(stringr)


scrape_links <- function(url, substring) {
  # Descargar la página con httr
  resp <- httr::GET(url)
  stop_for_status(resp)  # error si falla
  
  # Parsear el HTML
  page <- read_html(content(resp, "text"))
  
  # Extraer todos los <li>
  html_elements(page, "li") %>%
    # Convertir a data.frame directamente con map
    purrr::map_df(~{
      a <- html_element(.x, "a")
      if (!is.na(a)) {
        href <- html_attr(a, "href")
        if (!is.na(href) && str_detect(href, substring)) {
          tibble(
            texto = html_text(.x, trim = TRUE),
            url   = href
          )
        } else {
          tibble() # vacío
        }
      } else {
        tibble() # vacío
      }
    })
}



tiene_header <- function(link, colnames_fijas) {
  # Lee solo la primera fila
  primera <- tryCatch(
    fread(link, nrows = 1, header = FALSE, data.table = FALSE),
    error = function(e) return(NULL)
  )
  
  if (is.null(primera)) return(FALSE)
  
  # Comparar con los nombres esperados
  return(all(as.character(primera[1, ]) == colnames_fijas))
}


DEIS.extraer_links = function(tema = 'nacidosvivos'){
  
  # tema puede ser 'nacidosvivos' o 'defunciones'
  
  if(tema == 'nacidosvivos'){
    patron = 'nacweb'
  }  else if(tema == 'defunciones'){
    patron = 'defweb'
  } else{stop("El parámetro 'tema' debe ser 'nacidosvivos' o 'defunciones'")}
  
  # Define la URL base y la URL de la página a consultar
  url <- file.path("https://www.argentina.gob.ar/salud/deis/datos",tema)
  
  datos <- scrape_links(url, patron)
  
  datos$anio <- as.integer(str_extract(datos$texto, "AÑO (\\d{4}).*", group=1))
  
  return(datos)
}


DEIS.compilar <- function(links_df, colnames_fijas) {
  
  # Iterar sobre años y urls con purrr::map2
  res <- purrr::map2(
    .x = links_df$anio,
    .y = links_df$url,
    .f = function(year, link) {
      tryCatch({
        
        cat("Leyendo año", year, "\n")
        
        if (tiene_header(link, colnames_fijas)){
          c("El archivo ", link, " posee header ok\n")
          df <- fread(link)
        
          }else{
            c("El archivo ", link, " no posee header ok\n")
          df <- fread(link, header = F, col.names = colnames_fijas)
          }
        
        df$anio <- year
        return(df)
      },
      error = function(e) {
        message("Error leyendo año ", year, ": ", e$message)
        return(NULL)  # o un tibble vacío si preferís
      })
    }
  )
  
  # Nombrar la lista con los años
  names(res) <- links_df$anio
  
  return(res)
}



