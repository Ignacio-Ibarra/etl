library(httr2)
library(rvest)


UNWTO.bulk_download <- function() {
  
  page_url <- "https://www.untourism.int/tourism-statistics/tourism-statistics-database"
  
  # Emular navegador
  resp <- request(page_url) %>%
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
  
  match_idx <- grepl("UN_Tourism_bulk_data_download", links)
  filtered_links <- links[match_idx]
  
  return(filtered_links)
}





