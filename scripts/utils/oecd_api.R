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



oecd_api.download_data_from_url <- function(url, delimitador) {
  
  # Realiza la petición HTTP solicitando los datos en formato SDMX-CSV v1
  response <- GET(
    url,
    add_headers(
      Accept = "application/vnd.sdmx.data+csv; charset=utf-8; labels=both"
    )
  )
  
  # Verifica si la petición fue exitosa
  if (status_code(response) == 200) {
    # Convierte el contenido binario a texto
    contenido_texto <- rawToChar(content(response, "raw"))
    
    # Lee el contenido directamente como un dataframe
    datos <- read_delim(I(contenido_texto), delim = delimitador)
   
    
    return(datos)
    
  } else {
    stop(paste("Error:", status_code(response)))
  }
}


# url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_ALFS@DF_SUMTAB,1.0/all?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"
# delimiter <- ","  # Puede ser necesario ajustarlo según los datos reales
# 
# df <- oecd_api.download_data_from_url(url, delimiter)
# 
# if (!is.null(df)) {
#   print(head(df))
# }


