# Este programa es un wrapper de un wrapper de la API de sciencebase.gov
# Acá la documentación de la API https://www.usgs.gov/sciencebase-instructions-and-documentation/api-and-web-services
# Acá la documentación del paquete de R https://journal.r-project.org/archive/2016-1/winslow-chamberlain-appling-etal.pdf

library(sbtools)
library(dplyr)
library(purrr)
library(stringr)


# Función para extraer el año más cercano al año especificado (q_year) de cada título
extract_closest_year <- function(item, q_year) {
  # Extrae todos los números de 4 dígitos
  years <- str_extract_all(item$title, "\\b(20[0-9]{2})\\b") %>% 
    unlist() %>% 
    as.numeric()
  
  # Si hay años extraídos, calcula el más cercano al año q_year
  if (length(years) > 0) {
    closest_year <- years[which.min(abs(years - q_year))]
    return(list(item = item, closest_year = closest_year))
  } else {
    return(NULL)
  }
}


sciencebase.buscar_items <- function(folder_id, search_title = NULL, search_year = NULL){
  
  # Verificar que search_title no sea nulo
  if (is.null(search_title)) {
    stop("El argumento 'search_title' no puede ser NULL. Debe especificar un título de búsqueda o un patrón.")
  }
  
  childs <- sbtools::item_list_children(folder_id, limit = Inf) 
  
  filtered_childs <- childs %>% 
    keep(~ grepl(search_title, .x$title))
  
  if(!is.null(search_year)){
    
    filtered_childs <- filtered_childs %>%
      map(~ extract_closest_year(.x, search_year)) %>%
      compact()
    
    filtered_df <- tibble(
      item = map(filtered_childs, "item"),
      closest_year = map_dbl(filtered_childs, "closest_year")
    )
    
    
    closest_item <- filtered_df %>%
      filter(abs(closest_year - search_year) == min(abs(closest_year - search_year))) %>%
      pull(item)
    
    if (length(closest_item) == 1){
      return(closest_item[[1]])
    }else{
      return(closest_item)
    }
    
    
  }else{
    return(filtered_childs)
  }
  
}


sciencebase.base_archivos_de_item <- function(sbitem_obj, recursive, fetch_cloud_urls){
  
  archivos_df <- sbitem_obj$id %>% sbtools::item_list_files(., recursive = recursive, fetch_cloud_urls = fetch_cloud_urls)
  
  return(archivos_df)
}


sciencebase.filtrar_base_archivos <- function(base_df, pattern = NULL){
  results <- base_df %>% 
    dplyr::filter(grepl(pattern, fname)) %>% 
    select(fname, url)
  
  # Verificar que pattern no sea nulo
  if (is.null(pattern)) {
    stop("El argumento 'pattern' no puede ser NULL")
  }
  
  if (nrow(results) == 0) {
    stop("Con dicho patrón no se ha encontrado ningún archivo")
  }
  
  return(results)
  
}

