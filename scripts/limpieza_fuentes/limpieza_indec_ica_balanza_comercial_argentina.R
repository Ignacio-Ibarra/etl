# Función para obtener la ruta del archivo, compatible tanto en RStudio como en la consola
get_file_location <- function() {
  # Intenta obtener la ruta del archivo en RStudio
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    return(rstudioapi::getSourceEditorContext()$path)
  }
  
  # Alternativa para obtener la ruta si se usa source()
  this_file <- (function() { attr(body(sys.function(1)), "srcfile") })()
  
  # Si no se obtiene el path (e.g., en consola sin RStudio), asigna un valor por defecto
  if (!is.null(this_file)) {
    return(this_file$filename)
  } else {
    return("Archivo no especificado o ruta predeterminada")
  }
}

code_name <- get_file_location() %>% str_split_1(., "/") %>% tail(.,1)


id_fuente <- 269
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}


clean_serie_historica <- function(sheet_name, skip, columns_range){



  df_clean <- readxl::read_excel(argendataR::get_raw_path(fuente_raw),
                                sheet = sheet_name, 
                                col_names = F,
                             skip = skip) %>% 
    select(1,3:5)
  
  
  cols <- readxl::read_excel(argendataR::get_raw_path(fuente_raw),
                             sheet = sheet_name, 
                             range = columns_range,
                             col_names = F) 
  
  cols <- cols[1, ] %>% t() 
  cols <- cols[,1]
  cols <- cols[!is.na(cols)]


  names(df_clean) <- cols %>% janitor::make_clean_names() 

  # cuento cantidad de columnas
  num_cols <- length(cols)

  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(df_clean, num_cols-1)
  df_clean <- df_clean %>% 
    rename(anio = anos) %>% 
    dplyr::filter(!filter_bool)
  
  
  df_clean$anio <- str_extract(df_clean$anio, "(\\d{4}).*", group = 1) %>% as.integer()

return(df_clean)
  
}


sheet_name <- 'serie histórica'
columns_range <-  "A3:E3"
skip <- 7

df_clean <- clean_serie_historica(sheet_name = sheet_name, skip = skip, columns_range = columns_range)

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 136
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)