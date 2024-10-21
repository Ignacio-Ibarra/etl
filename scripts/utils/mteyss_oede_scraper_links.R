library(rvest)
library(xml2)


# Codigo para scrapear los links de esa parte
mteyss.oede.remuneracions_empleo_y_empresas = function(){
  
  page_oede <- "https://www.argentina.gob.ar/trabajo/estadisticas/oede-estadisticas-nacionales"
  
  # Leer el contenido HTML
  pagina <- read_html(page_oede)
  
  # Obtener el texto del h3 con id=1
  h3_relevante <- pagina %>%
    html_nodes(xpath = "//h3[@id='1']") %>%
    html_text(trim = TRUE)
  
  # Obtener todos los h4 relacionados con el h3
  h4_tags <- pagina %>%
    html_nodes(xpath = "//h3[@id='1']/following-sibling::h4[preceding-sibling::h3[@id='1']]")
  
  # Procesar cada h4 y su tabla relacionada
  resultados <- map_df(h4_tags, function(h4) {
    h4_texto <- html_text(h4, trim = TRUE)
    
    # Extraer todas las filas (tr) de la tabla relacionada con este h4
    filas_tabla <- h4 %>%
      html_node(xpath = "following-sibling::table[1]//tbody") %>%
      html_nodes("tr")
    
    # Extraer datos de cada fila
    map_df(filas_tabla, function(fila) {
      tibble(
        h3 = h3_relevante,
        h4 = h4_texto,
        detalle = html_node(fila, "td[data-label=' Detalle']") %>% html_text(trim = TRUE),
        fecha_publicacion = html_node(fila, "td[data-label='Actualización']") %>% html_text(trim = TRUE),
        link = html_node(fila, "td a") %>% html_attr("href")
      )
    })
  })
  
  # Asegurarse de completar los links relativos con la URL base
  resultados <- resultados %>%
    mutate(link = ifelse(!grepl("^http", link), paste0("https://www.argentina.gob.ar", link %>% str_remove(., ".*:#")), link) )
  
 return(resultados)
}



mteyss.oede.boletin_estadisticas_laborales_segun_sexo = function(){
  
  page_oede <- "https://www.argentina.gob.ar/trabajo/estadisticas/oede-estadisticas-nacionales"
  
  # Leer el contenido HTML
  pagina <- read_html(page_oede)
  
  # Obtener el texto del h3 con id=1
  h3_node <- pagina %>%
    html_nodes(xpath = "//h3[@id='3']")
  
  titulo <- h3_node %>%
    html_text(trim = TRUE)
  
  # Obtener todos los h4 relacionados con el h3
  fecha_pub <- h3_node %>%
    html_nodes(xpath = "following-sibling::h5[preceding-sibling::h3[@id='3']]") %>%
    html_text(trim = TRUE) %>% str_remove(., "Publicación - ")
  
  # Obtener el href del <a> dentro del <p> que sigue al h5
  link <- pagina %>%
    html_node(xpath = "//h3[@id='3']/following-sibling::h5[1]/following-sibling::p[1]/a") %>%
    html_attr("href")
  
  
  #
  resultados <- # Crear el data.frame con una única fila
    tibble(
      h3 = titulo,
      h4 = NA_character_,
      detalle = NA_character_,
      fecha_publicacion = fecha_pub,
      link = link
    )
  
  return(resultados)
}



# result <- mteyss.oede.boletin_estadisticas_laborales_segun_sexo()
# print(result)
# # A tibble: 1 × 5
# h3                                           h4    detalle fecha_publicacion link                                                            
# <chr>                                        <chr> <chr>   <chr>             <chr>                                                           
#   1 Boletín de estadísticas laborales según sexo NA    NA      Septiembre/2024   https://www.argentina.gob.ar/sites/default/files/boletin_estadi…
