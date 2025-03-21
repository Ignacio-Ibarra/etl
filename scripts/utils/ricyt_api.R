# Aquí se tiene el schema.json completo para armar el wrapper https://api.uis.unesco.org/api/public/openapi/schema.json
# Aquí se tiene la documentación https://api.uis.unesco.org/api/public/documentation/

library(httr)
library(rvest)
library(jsonlite)

# Definir la URL base de la API
url_api <- "https://app.ricyt.org/api"



RICYT.get_indicators <- function(patron = "comparative\\.html\\?indicator.*"){
  
    
  url_consulta <- "https://www.ricyt.org/category/indicadores/"
  
  # Obtiene el contenido de la página web
  web_content <- read_html(url_consulta)
  
  
  # Me quedo con los href
  links <- web_content %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    str_extract(., pattern = patron)
  
  text_links <- web_content %>%
    html_nodes("a") %>%
    html_text(trim = TRUE)
  
  # Filtra los links que contienen la URL base y aplican el pattern
  indicator_link <- links[grepl(patron, links)] %>% paste0("https://app.ricyt.org/ui/v3/",.)
  indicator <- text_links[grepl(patron, links)]
  id_indicator <- str_extract(indicator_link, pattern = "indicator=([^&]+)", group = 1)
  
  
  result <- data.frame(id_indicator, indicator, indicator_link)
  
  return(result)
  
  }



RICYT.get_indicator_data <- function(id_indicator){
  
  url <- glue::glue("{url_api}/comparative/ALL/2013,2022/{id_indicator}")
  
  response <- GET(url, add_headers("Content-Type:" = "application/json"))
  if (status_code(response) == 200) {
    
    resultado = list(url_consulta = url, response = fromJSON(content(response, as = "text")))
    return(resultado)
  } else {
    stop("Error al obtener los datos: ", status_code(response))
  }
}


# 
# indicadores <- RICYT.get_indicators()
# 
# 
# resultado <- RICYT.get_indicator_data(id_indicator = "GASIDSFPER")
