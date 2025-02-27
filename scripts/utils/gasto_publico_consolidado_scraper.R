library(rvest)
library(httr)


# nivel_gasto puede ser 'consolidado' 'nacional' 'provincial' o 'municipal'



get_links = function(url){
  
  
  # Obtiene el contenido de la página web
  web_content <- read_html(url)
  
  # Me quedo con los href
  tbody <- web_content %>% 
    html_nodes("tbody")
  
  fecha_actualizacion <- web_content %>% 
    html_nodes('p') %>% 
    html_text() %>% 
    purrr::keep(., ~all(grepl("Fecha de actualización:.*", .x))) %>% 
    str_remove(., "Fecha de actualización:") %>% 
    trimws(., which = "both") %>% 
    tools::toTitleCase(.)
  
  title <- web_content %>% 
    html_nodes('h3') %>% 
    html_text()
  
  
  tr <- tbody %>% 
    html_nodes('tr')
  
  td <- tr %>% 
    html_nodes('td')
  
  links <- td %>% 
    html_nodes('a') %>% 
    html_attr("href") %>% 
    str_replace(., "blank:#", "https://www.argentina.gob.ar")
  
  textos <- td %>% 
    html_text(., trim = T) %>% 
    purrr::keep(., ~all(.x!="Descargar"))
    
  table <- tbody %>% html_table()
  
  
  link_data <- data.frame(
    name = textos,
    url = links
  )
  
  
  result = list(
    titulo = title, 
    fecha_actualizacion = fecha_actualizacion,
    links  = link_data
    )
  
  return(result)
  
}

gasto_publico_consolidado.get_data_link = function(nivel_gasto = "consolidado"){
  
  url <- "https://www.argentina.gob.ar/economia/politicaeconomica/macroeconomica/gastopublicoconsolidado"
  
  result = get_links(url)
  
  
  result$links <- result$links %>% 
    dplyr::filter(grepl(nivel_gasto, name))
  
  
  return(result)
  
} 



# resultado <- gasto_publico_consolidado.get_data_link(nivel_gasto = "municipal")
