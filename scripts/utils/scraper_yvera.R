library(httr2)
library(rvest)
library(purrr)
library(stringr)
library(RSelenium)

YVERA.cuenta_satelite_turismo_links <- function(search_string) {
  
  page_url <- "https://www.yvera.tur.ar/sinta/informe/info/cuenta-satelite-de-turismo"
  
  # Emular navegador
  resp <- request(page_url) %>%
    req_options(ssl_verifypeer = 0) %>% 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36") %>%
    req_headers(
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    ) %>%
    req_perform()
  
  # Parsear el HTML
  web_content <- resp %>% resp_body_html()
  
  links <- web_content %>%
    html_elements("a") %>%
    html_attr("href")
  
  text_links <- web_content %>%
    html_elements("a") %>%
    html_text2()
  
  result <- data.frame(text = text_links, href = links) %>% 
    dplyr::filter(grepl(search_string, text)) %>% 
    mutate(url = glue::glue("https://www.yvera.tur.ar{href}"))
  
}


YVERA.tablero_serie_historica <- function(){
  
  page_url <- "https://tableros.yvera.tur.ar/turismo_internacional"
  
  # Emular navegador
  resp <- request(page_url) %>%
    req_options(ssl_verifypeer = 0) %>% 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36") %>%
    req_headers(
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    ) %>%
    req_perform()
  
  # Parsear el HTML
  web_content <- resp %>% resp_body_html()
  
  
}


YVERA.datos_abiertos <- function(slug){
  
  # ejemplo slug turismo-internacional-total-pais
  
  page_url <- glue::glue("https://datos.yvera.gob.ar/dataset/{slug}")
  
  # Emular navegador
  resp <- request(page_url) %>%
    req_options(ssl_verifypeer = 0) %>% 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36") %>%
    req_headers(
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    ) %>%
    req_perform()
  
  # Parsear el HTML
  web_content <- resp %>% resp_body_html()
  
  containers <- web_content %>% html_nodes("div.pkg-container")
  
  df <- map_df(containers, function(div) {
    
    # Extraer título y párrafo dentro de package-info
    title <- div %>% 
      html_node(".package-info h3") %>% 
      html_text(trim = TRUE)
    
    desc <- div %>% 
      html_node(".package-info p") %>% 
      html_text(trim = TRUE)
    
    texto <- str_c(title, ". ", desc)
    
    # Extraer el href del botón DESCARGAR
    download_href <- div %>% 
      html_nodes("a") %>% 
      keep(~ grepl("DESCARGAR", html_text(.x), ignore.case = TRUE)) %>% 
      html_attr("href") %>% 
      first()
    
    tibble(
      texto = texto,
      download_link = download_href
    )
  })
  
  df
  
    
  
  
}



CONECTIVIDAD_AEREA.datos_abiertos <- function(){
  
  
  page_url <- glue::glue("https://datos.gob.ar/dataset/turismo-conectividad-aerea")
  
  # Emular navegador
  resp <- request(page_url) %>%
    req_options(ssl_verifypeer = 0) %>% 
    req_user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36") %>%
    req_headers(
      "Accept-Language" = "en-US,en;q=0.9",
      "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8"
    ) %>%
    req_perform()
  
  # Parsear el HTML
  web_content <- resp %>% resp_body_html()
  
  containers <- web_content %>% html_nodes("div.pkg-container")
  
  df <- map_df(containers, function(div) {
    
    # Extraer título y párrafo dentro de package-info
    title <- div %>% 
      html_node(".package-info h3") %>% 
      html_text(trim = TRUE)
    
    desc <- div %>% 
      html_node(".package-info p") %>% 
      html_text(trim = TRUE)
    
    texto <- str_c(title, ". ", desc)
    
    # Extraer el href del botón DESCARGAR
    download_href <- div %>% 
      html_nodes("a") %>% 
      keep(~ grepl("DESCARGAR", html_text(.x), ignore.case = TRUE)) %>% 
      html_attr("href") %>% 
      first()
    
    tibble(
      texto = texto,
      download_link = download_href
    )
  })
  
  df
  
  
  
  
}

