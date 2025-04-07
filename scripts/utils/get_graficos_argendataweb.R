urls <- list("https://argendata.fund.ar/topico/emisiones-de-gases-de-efecto-invernadero/",
          "https://argendata.fund.ar/notas/que-es-la-intensidad-de-carbono/")


inner_get_ids_graficos <- function(url) {
  
  assertthat::is.string(url)
  
  resp <- httr2::request(url) %>% 
    httr2::req_perform()
  
  assertthat::assert_that(resp %>% 
                            httr2::resp_status() == "200")
  

  assertthat::assert_that(  resp %>% 
                              httr2::resp_content_type() == "text/html")  
  
  ids <- resp$body %>% 
    rvest::read_html() %>% 
    rvest::html_elements(css = ".chart.grafico_argendata") %>% 
    rvest::html_attr("id") %>% 
    str_remove_all(., "wrapper_grafico_")
  
  ids
}

get_ids_graficos <- function(url) {
  
  ids <- lapply(url, inner_get_ids_graficos)

  ids <- lapply(ids, sort)  
  
  unlist(ids)
}

get_ids_graficos(urls)
