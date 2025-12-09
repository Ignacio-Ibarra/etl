library(httr2)

# slug_recurso <- "arribos-de-turistas"

MAR_DEL_PLATA.datos_abiertos <- function(slug_recurso){
  
  # ejemplo slug turismo-internacional-total-pais
  
  page_url <- glue::glue("https://datos.mardelplata.gob.ar/?q=dataset/{slug_recurso}")
  
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
  
  container <- web_content %>% 
    html_nodes("div#data-and-resources")
  
  
  main_title <- container %>% 
    html_node("a") %>% 
    html_attr("title")
  
  
  link <- container %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    purrr::keep(., ~all(grepl("\\.csv$", .x)))
  
  subtitle <- container %>% 
    html_nodes("p") %>% 
    html_text() %>% 
    purrr::keep(., ~all(.x != ""))
  
 
  title <- glue::glue("{main_title}. {subtitle}")
  
  
  # Buscar la fila <tr> que contiene un <th> con texto "Fuente"
  fuente <- web_content %>% 
    html_nodes("tr") %>% 
    purrr::keep(~ {
      th <- html_node(.x, "th")
      !is.null(th) && trimws(html_text(th)) == "Fuente"
    }) %>% 
    html_node("td div.field-item.even") %>% 
    html_text(trim = TRUE)
  
  
  result <- list(title = title, url = link, fuente = fuente)
  
  
  
}





