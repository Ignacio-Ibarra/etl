# cedlas ---------------------------------------------------------------


# descarga

url_req <-  httr2::request("https://www.cedlas.econo.unlp.edu.ar/wp/en/estadisticas/isa/")
web_cedlas <- httr2::req_perform(url_req)

links <- web_cedlas$body %>% 
  xml2::read_html() %>% 
  xml2::xml_find_all("//*[contains(@class, 'vc_btn3-size-md')]") %>% 
  xml2::xml_attr("href") %>% 
  unique()

links <- paste0("https://www.cedlas.econo.unlp.edu.ar", links)  


for(l in links) {
  
  name <- gsub("https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/", "", l) %>% 
    tolower() %>%
    janitor::make_clean_names() %>%
    gsub("_xlsx", ".xlsx", .)
  
  download.file(l,
                mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
                destfile = sprintf("data/_FUENTES/raw/%s", name)
  )
  
  print(l)
  
  # agregar_fuente_raw(
  #   nombre = paste("Indicadores Sociales de Argentina", gsub(".xlsx","", name), sep = "-"),
  #                url = l,
  #                institucion = "Centro de Estudios Distributivos, Laborales y Sociales",
  #                path_raw = name,
  #                actualizable = T,
  #                fecha_descarga = Sys.Date(),
  #                fecha_actualizar = "",
  #                script = "descarga_cedlas.R")
  
  
  id <- fuentes_raw() %>%
    filter(url == l) %>%
    pull(id_fuente)
  
  actualizar_fuente(id_fuente = id, fecha_descarga = Sys.Date())
  
}

