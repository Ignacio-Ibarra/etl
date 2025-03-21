library(rvest)
library(httr)


#solo esta implementado el scrapeo de Cadenas Productivas

URL_GENERAL <- "https://www.econo.unlp.edu.ar/laboratorio/"

UNLP_FCE_LAB.get_links_cadenas_productivas <- function() {
  
  url <- file.path(URL_GENERAL, "cadenas-productivas-9299")
  
  # Obtiene el contenido de la página web
  web_content <- read_html(url)
  
  # Obtiene los href y el texto de cada enlace
  links <- web_content %>%
    html_nodes("a") %>%
    html_attr("href")
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
 
  # Filtrar solo los links que terminan en extensiones de archivo comunes
  df_archivos <- data.frame(text_links, links, stringsAsFactors = FALSE) %>% 
    filter(grepl("(drive\\.google\\.com|\\.(pdf|xls|xlsx|csv|docx?|zip|rar)$)", links, ignore.case = TRUE)) %>%
    group_by(links) %>% 
    summarise(text_links = trimws(paste0(text_links, collapse = " "))) %>% 
    ungroup() %>% 
    mutate(links = case_when(
      str_detect(links, "^https://") ~ links,
      str_detect(links, "^/frontend.*") ~ paste0("https://www.econo.unlp.edu.ar", links, collapse = ""),
      TRUE ~ NA_character_
    ))
  
  return(df_archivos)
}




