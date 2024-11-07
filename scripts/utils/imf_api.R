# Este es un wrapper de la API del FMI. 
# La documetanción para utilizar el JSON RESTful Web Service 
# se puede encontrar aquí https://datahelp.imf.org/knowledgebase/articles/667681-using-json-restful-web-service

library(httr)
library(jsonlite)

URL_BASE <- "http://dataservices.imf.org/REST/SDMX_JSON.svc"


get_response_json <- function(url){
  
  response <- GET(url)
  
  # Verificar el estado de la respuesta
  if (status_code(response) != 200) {
    stop("Error en la solicitud: ", status_code(response))
  }
  
  requested_url <- response$url
  # Parsear el contenido a JSON y convertirlo a data frame
  content <- content(response, as = "text")
  json_data <- fromJSON(content)
  
  return(json_data)
}


json_to_df <- function(json_data){
  
  # Convertir el JSON a data frame
  df <- as.data.frame(json_data)
  
  return(df)
}



imf.get_available_datasets <- function(){
  endpoint = "/Dataflow"
  url = paste0(URL_BASE, endpoint)
  cat(url)
  json_data <- get_response_json(url)
  result = json_to_df(json_data)
  ids <- result$Structure.Dataflows.Dataflow.KeyFamilyRef$KeyFamilyID
  names <- result$Structure.Dataflows.Dataflow.Name$`#text`
  descriptions <- result$Structure.Dataflows.Dataflow.Description$`#text`
  available_datasets <- data.frame(ids, names, descriptions)
  return(available_datasets)
}


# imf.get_codelist <- function(database_id, codelist_code){
#   endpoint = glue::glue("/CodeList/{codelist_code}|{database_id}")
#   url = paste0(URL_BASE, endpoint)
#   cat(url)
#   json_data <- get_response_json(url)
#   return(json_data)
# }



imf.get_data_structure <- function(database_id){
  
  endpoint = glue::glue("/DataStructure/{database_id}")
  url = paste0(URL_BASE, endpoint)
  cat(url)
  json_data <- get_response_json(url)
  return(json_data)
}

# Devuelve un data.frame
imf.get_compact_data <- function(database_id){
  
  endpoint = glue::glue("/CompactData/{database_id}")
  url = paste0(URL_BASE, endpoint)
  cat(url)
  json_data <- get_response_json(url)
  data <- json_data$CompactData$DataSet$Series %>%  unnest(Obs)
  return(list(url = url, data = data))
}



imf.get_metadata <- function(database_id){
  
  endpoint = glue::glue("/GenericMetadata/{database_id}")
  url = paste0(URL_BASE, endpoint)
  cat(url)
  json_data <- get_response_json(url)
  
  metadata <- json_data$GenericMetadata$MetadataSet$AttributeValueSet$ReportedAttribute
  metadata_df <- data.frame()
  
  for (i in 1:length(metadata)){
    sub_a <- metadata[[i]]
    sub_sub_a <- sub_a$ReportedAttribute
    dimension <- sub_sub_a[[1]]$`@conceptID`
    dimension_data <- sub_sub_a[[2]] %>% unnest(., Value)
    dimension_data$dimension <- dimension
    metadata_df <- metadata_df %>% bind_rows(dimension_data)
    
  }
  
  
  # Crear la lista de data.frames, donde los nombres de la lista serán los valores únicos de la columna
  lista_df <- split(metadata_df, metadata_df$dimension)
  
  
  return(list(url = url, data = lista_df))
  
}


# compact_data <- imf.get_compact_data("PCPS")
# 
# lista_metadata <- imf.get_metadata("PCPS")
# 
# cm <- lista_metadata %>% pluck("COMMODITY") %>% select(-ends_with("lang"))
# 
# v <- cm %>% select(-ends_with("lang")) %>% 
#   mutate(row_id = 1:nrow(cm)) %>% 
#   pivot_wider(id_cols = c(row_id, dimension), names_from = `@conceptID`, values_from = `#text`)
# 
# groups <- c()
# g <- 0
# for (i in cm$`@conceptID`){
#   if (i == "COMMODITY_NAME"){
#     g <- g + 1
#   }
#   groups <- c(groups, g)
# }
# 
# cm$group <- groups
# 
# 
# v <- cm %>% pivot_wider(id_cols = c(group, dimension), 
#                         names_from = `@conceptID`, 
#                         values_from = `#text`) %>% 
#   janitor::clean_names() %>% 
#   select(group:commodity_topic)
