# Wrapper de OECD API 
# Docs https://gitlab.algobank.oecd.org/public-documentation/dotstat-migration/-/raw/main/OECD_Data_API_documentation.pdf

# Hay que entrar al Data Explorer https://data-explorer.oecd.org/ 
# buscar lo que se quiere descargar y extraer la URL de la solapa Devloper API

# el siguiente codigo toma la URL y descarga el archivo como csv


library(httr)
library(readr)
library(xml2)



#' Obtener la lista de datasets disponibles en la API de la OECD
#' @return Un dataframe con los datasets disponibles
oecd_api.get_datasets <- function() {
  url <- "https://sdmx.oecd.org/public/rest/dataflow/all"
  response <- httr::GET(url)
  
  if (status_code(response) == 200) {
    content_xml <- content(response, as = "text", encoding = "UTF-8")
    
    doc <- xml2::read_xml(content_xml)
    
    # Obtener namespaces
    namespaces <- xml_ns(doc)
    
    # Extraer los identificadores, nombres y agencyID de los datasets
    datasets <- xml_find_all(doc, "//structure:Dataflow", ns = namespaces)
    
    dataset_list <- lapply(datasets, function(ds) {
      id <- xml_attr(ds, "id")
      agencyID <- xml_attr(ds, "agencyID")
      title <- xml_text(xml_find_first(ds, ".//common:Name[@xml:lang='en']", ns = namespaces))
      structure_query <- glue::glue("https://sdmx.oecd.org/public/rest/dataflow/{agencyID}/{id}/?references=all")
      list(id = id, agencyID = agencyID, title = title, structure_query = structure_query)
    })
    
    return(bind_rows(dataset_list))
  } else {
    stop(paste("Error en la solicitud:", status_code(response)))
  }
}



oecd_api.download_data_from_url <- function(url, delimitador, verbose=FALSE) {
  
  progress = FALSE
  show_col_types = FALSE
  if (verbose){
    progress = TRUE
    show_col_types = TRUE
  }
  
  response <- GET(
    url,
    add_headers(
      `Accept` = "application/vnd.sdmx.data+csv; charset=utf-8; labels=both",
      `Accept-Encoding`= "gzip, deflate, br"
    )
  )
  
  print(headers(response)$`retry-after`)
  
  if (status_code(response) == 200) {
    contenido_texto <- content(response, "text", encoding = "UTF-8")
    
    datos <- tryCatch({
      read_delim(I(contenido_texto), delim = delimitador, progress = progress, show_col_types = show_col_types)
    }, error = function(e) {
      stop("Error al leer los datos: ", e$message)
    })
    
    return(datos)
  } else {
    stop(paste("Error:", status_code(response)))
  }
}


oecd_api.download_data_with_params = function(agencyID=NULL, indicatorID=NULL, start_year=NULL, end_year=NULL, data_selection_str='all', verbose = FALSE){
  
  if (any(sapply(list(agencyID, indicatorID, start_year, end_year), function(x) is.null(x) || is.na(x)))){
    stop("Ninguno de los parámetros: agencyID, indicatorID, start_year y end_year deben ser nulos")
  }
  
  years <- start_year:end_year
  
  downloads <- purrr::map(
    years, function(q_year){
      url <- glue::glue("https://sdmx.oecd.org/public/rest/data/{agencyID},{indicatorID},1.0/{data_selection_str}?startPeriod={q_year}&endPeriod={q_year}&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
      str_msg = glue::glue("Se ha descargado el año {q_year}")
      message(str_msg)
      oecd_api.download_data_from_url(url, delimitador = ",", verbose = verbose)}
  )
  
  url_general <- glue::glue("https://sdmx.oecd.org/public/rest/data/{agencyID},{indicatorID},1.0/{data_selection_str}?dimensionAtObservation=AllDimensions&format=csvfilewithlabels")
  return(list(url = url_general, descargas = downloads))
  
}


# url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_ALFS@DF_SUMTAB,1.0/all?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
# delimiter <- ","  # Puede ser necesario ajustarlo según los datos reales
# 
# df <- oecd_api.download_data_from_url(url, delimiter)
# 
# if (!is.null(df)) {
#   print(head(df))
# }


