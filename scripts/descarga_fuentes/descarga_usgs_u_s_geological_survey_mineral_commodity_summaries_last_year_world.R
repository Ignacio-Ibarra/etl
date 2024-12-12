code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-03-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/usgs_sciencebase_api.R")


folder_id <- "5c8c03e4e4b0938824529f7d" #National Minerals Information Center

search_title <- "U.S. Geological Survey Mineral Commodity Summaries"

search_year <- as.numeric(format(Sys.Date(), "%Y"))

search_file <- "world.zip"

sbitem_obj <- sciencebase.buscar_items(folder_id = folder_id, search_title = search_title, search_year = search_year)

fuente_nombre <- sbitem_obj$title

search_file_metadata <- sbitem_obj$files %>% 
  purrr::keep(., function(x){x$name == search_file}) 

search_file_metadata <- search_file_metadata[[1]]

url <- search_file_metadata$url

file_title <- search_file_metadata$title

titulo <- glue::glue("{fuente_nombre} - {file_title}")

download_filename <- glue::glue("{file_title %>% janitor::make_clean_names()}_{search_file}")

destfile <- glue::glue("{tempdir()}/{download_filename}")

fraw <- fuentes_raw()

if(!(url %in% fraw$url)){
  
  
  download.file(url, destfile, mode = "wb")
  agregar_fuente_raw(url = url,
                     nombre = titulo,
                     institucion = "National Minerals Information Center",
                     actualizable = T,
                     script = code_name,
                     api = T, 
                     path_raw = download_filename,
                     fecha_actualizar = fecha_actualizar)
  
}else{
  
  id_fuente <- fraw[fraw$url == url, c('id_fuente')] %>% drop_na(id_fuente) %>% pull()
  download.file(url, destfile, mode = "wb")
  actualizar_fuente_raw(id_fuente = id_fuente,
                     url = url,
                     actualizable = T,
                     script = code_name,
                     api = T, 
                     path_raw = download_filename,
                     fecha_actualizar = fecha_actualizar)
 
}


