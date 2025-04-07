library(httr)
library(rvest)


SCIMAGO.get_filters <- function(){
  
  url_consulta <- "https://www.scimagoir.com/rankings.php"
  
  web_content <- read_html(url_consulta) 
  
  
  # Extraer los elementos <li> dentro de <ul> dentro de <div class="dropdown">
  dropdown_items <- web_content %>%
    html_nodes("div.dropdown ul li")
  
  # Extraer los datos de los tags <a> dentro de <li>
  dropdown_data <- dropdown_items %>%
    html_node("a") %>%
    { tibble(
      href = html_attr(., "href"),
      valor_filtro = html_attr(., "data-code"),
      contenido_filtro = html_text(.)
    ) } %>%
    mutate(
      nombre_filtro = str_extract(href, "(?<=\\?).*?(?==)")
    ) %>% 
    drop_na(href) %>% 
    select(nombre_filtro, contenido_filtro, valor_filtro)
  
  
  return(dropdown_data)
  
}


SCIMAGO.get_data <- function(ranking = "", sector = "all", area = "all", country = "all", year) {
  
  
  filtros = SCIMAGO.get_filters()
  
  if (!is.numeric(year) || any(is.na(as.integer(year)))) {
    stop("'year' debe ser un entero o un vector de enteros")
  }
  
  country <- filtros %>% 
    dplyr::filter(contenido_filtro %in% country) %>% 
    dplyr::pull(valor_filtro) %>% 
    stringr::str_replace_all("\\s+", "%20") %>% 
    paste0(collapse = ",") 
  
  anios_filtro <- filtros %>% 
    dplyr::filter(contenido_filtro %in% as.character(year)) %>% 
    dplyr::rename(year = contenido_filtro)
  
  responses <- purrr::map2(anios_filtro$year, anios_filtro$valor_filtro, function(year, anio_filtro) {
    
    url <-  glue::glue("https://www.scimagoir.com/getdata.php?rankingtype={ranking}&sector={sector}&area={area}&country={country}&year={anio_filtro}&top=0&format=csv&type=download")
    print(url)
    response <- httr::GET(url)
    
    if (httr::status_code(response) != 200) {
      stop(glue::glue("Error en la solicitud para {url}, código de estado: {httr::status_code(response)}"))
    }
    
    content_text <- httr::content(response, as = "text", encoding = "UTF-8")
    df <- read.csv(text = content_text, sep = ";")
    df$year <- as.integer(year)  
    return(df)
  })
  
  final_df <- dplyr::bind_rows(responses)  
  
  return(final_df)
}



SCIMAGO.get_ranking_evolution <- function(sector = "all", area = "", country = "all", start_year){
  
  
  filtros = SCIMAGO.get_filters()
  
  if (!is.numeric(start_year) || any(is.na(as.integer(start_year)))) {
    stop("'year' debe ser un entero o un vector de enteros")
  }
  
  country <- filtros %>% 
    dplyr::filter(contenido_filtro %in% country) %>% 
    dplyr::pull(valor_filtro) %>% 
    stringr::str_replace_all("\\s+", "%20") %>% 
    paste0(collapse = ",") 
  
  url <- glue::glue("https://www.scimagoir.com/getdatarankevolution_new.php?area={area}&sector={sector}&country={country}&startyear={start_year}")
  
  print(url)
  response <- httr::GET(url)
  
  if (httr::status_code(response) != 200) {
    stop(glue::glue("Error en la solicitud para {url}, código de estado: {httr::status_code(response)}"))
  }
  
  content_text <- httr::content(response, as = "text", encoding = "UTF-8")
  df <- read.csv(text = content_text, sep = ";")
  
  return(df)
  
}

# ejemplo <- SCIMAGO.get_ranking_evolution(sector = "Government", country = "Latin America", start_year = 2009)
# 
# v <- ejemplo %>% 
#   dplyr::filter(Year == 2009 | Year == 2024)
