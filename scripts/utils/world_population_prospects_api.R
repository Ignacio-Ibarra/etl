library(jsonlite)
library(httr)

WPP_api_call <- function(relative_path, topics_list = FALSE) {
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, relative_path)
  cat(target)
  
  # Realizo la solicitud con httr::GET
  response <- GET(target, timeout(10000L))
  
  # Verifico el código de estado de la respuesta
  status_code <- status_code(response)
  
  if (status_code != 200) {
    # Imprime el código de estado y el contenido de la respuesta en caso de error
    cat("Error: HTTP", status_code, "\n")
    cat("Response content:", content(response, as = "text"), "\n")
    stop("La API devolvió un código de error.")
  }
  
  # Intenta parsear el contenido como JSON
  content_text <- content(response, as = "text")
  response_json <- tryCatch(fromJSON(content_text), error = function(e) {
    stop("Error al parsear la respuesta JSON: ", e$message)
  })
  
  # Verifico si la respuesta es una lista (posible paginación)
  if (class(response_json) == "list") {
    df <- response_json$data
    while (!is.null(response_json$nextPage)) {
      next_response <- GET(response_json$nextPage)
      response_json <- fromJSON(content(next_response, as = "text"))
      df_temp <- response_json$data
      df <- rbind(df, df_temp)
    }
    return(list(url = target, data = df))
  } else {
    # Carga directa de datos sin paginación
    if (topics_list == TRUE) {
      df <- fromJSON(content_text, flatten = TRUE)
      return(df[[5]][[1]])
    } else {
      df <- fromJSON(content_text)
      return(list(url = target, data = df))
    }
  }
}


WPP_get_locations <- function() {
  base_url <- "https://population.un.org/dataportalapi/api/v1"
  target <- paste0(base_url, "/locations")
  cat("Consultando:", target, "\n")
  
  # Primer request con fromJSON
  response_json <- tryCatch(
    jsonlite::fromJSON(target),
    error = function(e) stop("Error al parsear la respuesta JSON: ", e$message)
  )
  
  # Si es una lista (paginado)
  if (is.list(response_json) && "data" %in% names(response_json)) {
    df <- response_json$data
    
    # Loop hasta que no haya más páginas
    while (!is.null(response_json$nextPage)) {
      next_page_string <- str_extract(response_json$nextPage, "(locations.*)", group = 1)
      response_json <- jsonlite::fromJSON(file.path(base_url, next_page_string))
      df_temp <- response_json$data
      df <- rbind(df, df_temp)
    }
  }
    
    return(list(url = target, data = df))
    
}



WPP_get_data <- function(indicators, locations, start = NULL, end = NULL){
  
  indicators <- paste0(indicators, collapse = ",")
  locations <- paste0(locations, collapse = ",")
  
  start <- ifelse(is.null(start), 1950, start)
  end <- ifelse(is.null(end), year(Sys.Date())-1, end)
  
  target <- paste0("/data/indicators/",indicators,"/locations/",locations,"/start/",start,"/end/",end,"?sort=id")
  
  WPP_api_call(target)
  
}



# country_codes <- paste(df_locations$id, collapse = ",")
# 
# indicator_metadata <- WPP_api_call("/indicators/Ex1?sort=id")
# indicator_code <- indicator_metadata$id
# 
# start_year <- indicator_metadata$sourceStartYear
# 
# end_year <- indicator_metadata$sourceEndYear
# 
# target <- paste0("/data/indicators/",indicator_code,"/locations/","4,8,12,16","/start/",start_year,"/end/",end_year)

# # Identifies ID code for Western Africa
# western_africa_id <- df_locations[df_locations$name=="Western Africa", "id"]
# 
# # Restricts the dataframe to only include geographies from Western Africa
# country_codes <- c(4,8,12)
# country_codes <- paste(country_codes, collapse = ",")
# 
# # Uses callAPI function to get a list of only Family Planning indicators
# indicator_metadata <- WPP_api_call("/indicators/Ex1?sort=id")
# indicator_codes <- indicator_metadata$id
# indicator_codes <- paste(indicator_codes, collapse = ",")
# 
# target <- paste0("/data/indicators/",indicator_codes,"/locations/",country_codes,"/start/2020/end/2020")
# 
# wpp_df <- WPP_api_call(target)