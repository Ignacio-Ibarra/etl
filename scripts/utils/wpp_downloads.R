library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)

base_url <- "https://population.un.org/wpp"

clean_html <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (length(x) == 0) return(NA_character_)
  
  # Parsear el html
  doc <- read_html(x)
  
  # Extraer párrafos o, si no hay, todo el texto
  paragraphs <- html_nodes(doc, "p") %>% html_text(trim = TRUE)
  
  if (length(paragraphs) == 0) {
    text <- html_text(doc, trim = TRUE)
  } else {
    text <- paste(paragraphs, collapse = ". ")
  }
  
  # Eliminar espacios dobles y saltos de línea
  text <- str_squish(text)
  
  return(text)
}

# Función principal
json_to_dataframe <- function(json_path) {
  json_data <- jsonlite::fromJSON(json_path, simplifyVector = FALSE)
  
  # Función auxiliar para extraer los archivos
  extract_files <- function(folder_name, major_group, subgroup_name, item) {
    tibble(
      Folder = folder_name,
      MajorGroup = major_group$name,
      MajorGroup_Header = clean_html(major_group$Header),
      SubGroup = subgroup_name,
      Item_Description = item$Description %||% NA_character_,
      File_Type = map_chr(item$File, ~ .x$type %||% NA_character_),
      File_Path = map_chr(item$File, ~ .x$Path %||% NA_character_),
      File_Title = map_chr(item$File, ~ .x$Title %||% NA_character_),
      download_url = file.path(base_url, File_Path) %>% utils::URLencode()
    )
  }
  
  # Recorrer toda la jerarquía
  df <- map_dfr(json_data$Folders, function(folder) {
    map_dfr(folder$MajorGroup, function(mg) {
      map_dfr(mg$SubGroup, function(sg) {
        map_dfr(sg$Item, ~ extract_files(folder$name, mg, sg$name, .x))
      })
    })
  })
  
  return(df)
}


WPP_get_data_links = function(){
  
  url <- file.path(base_url, "assets/downloads.json")
  
  json_to_dataframe(url)
  
  
}