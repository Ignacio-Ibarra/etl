library(rvest)
library(httr)
library(DBI)
library(duckdb)
library(data.table)




URL_CONSULTA <- "https://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37"

BACI.get_download_links <- function() {
  
  page <- read_html(URL_CONSULTA)
  
  # Encontrar el div con class "content-box" que contiene el texto buscado
  content_boxes <- page %>% html_elements("div.content_box")
  
  target_div <- content_boxes %>% 
    keep(~ html_text(.x) %>% str_detect("Download")) %>% 
    first()
  
  if (is.null(target_div)) {
    stop("No se encontró el div con el texto 'Download'.")
  }
  
  # Extraer la versión dentro del tag <em>
  version <- target_div %>% 
    html_element("em") %>% 
    html_text()
  
  # Extraer los elementos <li> dentro del <ul>
  items <- target_div %>% html_elements("ul li")
  
  data <- tibble(
    text = items %>% map_chr(~ html_text(.x)),
    link = items %>% map_chr(~ html_element(.x, "a") %>% html_attr("href")),
    version = version
  )
  
  return(data)
}


# Definir la función para crear la base de datos
BACI.create_db <- function(zip_file, db_file) {
  # Crear una carpeta temporal para extraer los archivos
  temp_dir <- tempdir()
  curr_date <- format(Sys.time(), "%Y%m%d")
  extract_dir <- file.path(temp_dir, glue::glue("db_files_{curr_date}"))
  
  # Eliminar el directorio si ya existe
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE)
  }
  
  # Crear el directorio
  dir.create(extract_dir)
  unzip(zip_file, exdir = extract_dir)
  
  # Listar los archivos CSV en la carpeta temporal
  csv_files <- list.files(extract_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Conectar a DuckDB, especificando el archivo de base de datos local
  con <- dbConnect(duckdb::duckdb(), dbdir = db_file)
  
  # Leer y cargar los archivos CSV en tablas de DuckDB
  for (csv_file in csv_files) {
    print(csv_file)
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    
    if (grepl("product_codes.*", table_name)) {
      data <- fread(csv_file) %>% 
        mutate(code = str_pad(code, width = 6, side = "left", pad = "0")) 
      dbWriteTable(con, "productos", data, overwrite = TRUE)
      
    } else if (grepl("country_codes.*", table_name)) {
      data <- fread(csv_file)
      dbWriteTable(con, "paises", data, overwrite = TRUE)
    } else {
      # Leer el archivo CSV de comercio
      comercio_data <- fread(csv_file)
      
      # Si la tabla de comercio no existe, crearla
      if (!dbExistsTable(con, "comercio")) {
        dbWriteTable(con, "comercio", comercio_data, overwrite = TRUE)
      } else {
        # Si la tabla de comercio ya existe, agregar los datos
        dbAppendTable(con, "comercio", comercio_data)
      }
    }
    cat(table_name, "...creada\n\n")
  }
  
  # Cerrar la conexión
  dbDisconnect(con, shutdown = TRUE)
}

BACI.get_db_from_zip <- function(zip_file) {
  # Crear una carpeta temporal para extraer los archivos
  temp_dir <- tempdir()
  curr_date <- format(Sys.time(), "%Y%m%d")
  extract_dir <- file.path(temp_dir, glue::glue("db_files_{curr_date}"))
  
  # Eliminar el directorio si ya existe
  if (dir.exists(extract_dir)) {
    unlink(extract_dir, recursive = TRUE)
  }
  
  # Crear el directorio
  dir.create(extract_dir)
  unzip(zip_file, exdir = extract_dir)
  
  # Listar los archivos CSV en la carpeta temporal
  csv_files <- list.files(extract_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Conectar a DuckDB, especificando el archivo de base de datos local
  db_file <- tempfile(fileext = ".duckdb")
  con <- dbConnect(duckdb::duckdb(), dbdir = db_file)
  
  # Leer y cargar los archivos CSV en tablas de DuckDB
  for (csv_file in csv_files) {
    print(csv_file)
    table_name <- tools::file_path_sans_ext(basename(csv_file))
    
    if (grepl("product_codes.*", table_name)) {
      data <- fread(csv_file)
      dbWriteTable(con, "productos", data, overwrite = TRUE)
      
    } else if (grepl("country_codes.*", table_name)) {
      data <- fread(csv_file)
      dbWriteTable(con, "paises", data, overwrite = TRUE)
    } else {
      # Leer el archivo CSV de comercio
      comercio_data <- fread(csv_file)
      
      # Si la tabla de comercio no existe, crearla
      if (!dbExistsTable(con, "comercio")) {
        dbWriteTable(con, "comercio", comercio_data, overwrite = TRUE)
      } else {
        # Si la tabla de comercio ya existe, agregar los datos
        dbAppendTable(con, "comercio", comercio_data)
      }
    }
    cat(table_name, "...creada\n\n")
  }
  
  # Devolver
  return(con)
}

extract_to_dir <- function(zip_file, filename, exdir = tempdir()){
  unzip(zip_file, file = filename, exdir = exdir)
  return(file.path(exdir, filename))
}

BACI.extract_and_create_table <- function(zip_file, csv_file, con, table_name) {
  
  path = extract_to_dir(zip_file, filename = csv_file)
  
  data <- fread(path)
  dbWriteTable(con, table_name, data, overwrite = TRUE)
      
}








BACI.get_db <- function(db_path){
  
  con <- dbConnect(duckdb::duckdb(), dbdir = db_file)
  return(con)
  
}




BACI.get_table <- function(con, table_name){
  
  table <- tbl(con, table_name)
  
  if (table_name == "productos"){
    
    table <- table %>% 
      mutate(code = str_pad(as.character(code), width = 6, side = "left", pad = "0"))
    
  }
  if (table_name == "comercio"){
    
    table <- table %>% 
      mutate(k = str_pad(as.character(k), width = 6, side = "left", pad = "0"))
    
  }
  
  
  return(table)
 
}

# # Crear un archivo temporal para la base de datos
# db_file <- tempfile(fileext = ".duckdb")
# zip_file <- argendataR::get_raw_path("R336C0")
# 
# BACI.create_db(zip_file = zip_file, db_file = db_file)
# 
# 
# # Verificar las tablas creadas (reconectar para verificar)
# con <- dbConnect(duckdb::duckdb(), dbdir = db_file)
# dbListTables(con)
# dbDisconnect(con, shutdown = TRUE)
