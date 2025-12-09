library(httr2)
library(rvest)
library(purrr)
library(stringr)

CANCILLERIA.centro_economia_internacional_links <- function() {
  
  page_url <- "https://cancilleria.gob.ar/es/cei/estadisticas"
  
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
    dplyr::filter(grepl("\\.(xls$|csv$|xlsx$)", href)) 
  
}