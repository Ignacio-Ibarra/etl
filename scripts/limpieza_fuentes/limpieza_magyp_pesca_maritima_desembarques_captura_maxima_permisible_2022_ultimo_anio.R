#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 332
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

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}





clean_sheet <- function(worksheet, sheet_name, skip, filas_columnas, anio){
  
  sheet <- worksheet[[sheet_name]]
  
  filas_columnas <- which(sheet$...1 == "Especie")
  
  skip <- filas_columnas
  
  cols_ <- sheet %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  min_id <- skip + 1
  max_id <- skip + 11
  # Leo datos
  sheet_data <- sheet %>% dplyr::filter(1:nrow(sheet) %in% min_id:max_id)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  cols <- cols$concatenado
  
  names(sheet_data) <- cols
  
  df <- sheet_data %>% 
    mutate(anio = anio) %>% 
    janitor::clean_names() 
  
  
  return(df)
  
}



clean_worksheet <- function(lista_data, inner_file, skip, filas_columnas){
  
  anio <- str_extract(inner_file, "\\d{4}") %>% as.integer()
  
  worksheet <- lista_data[[inner_file]]
  
  sheets <- names(worksheet)
  
  data_anual <- purrr::map_dfr(sheets, function(sheet_name){ 
    clean_sheet(worksheet = worksheet,
                sheet_name = sheet_name,
                anio = anio)
    }
  ) 
  
  return(data_anual)
  
}


json_data_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(., simplifyVector = TRUE)


inner_files <- names(json_data_raw)

df_stage <- purrr::map_dfr(inner_files, function(inner_file){
  clean_worksheet(lista_data = json_data_raw,
                  inner_file = inner_file)
  }
)


df_clean <- df_stage %>% 
  mutate(area =ifelse(area == "--", NA, area),
         cmp = as.numeric(cmp),
         capturas = as.numeric(capturas),
         percent = as.numeric(percent))

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 207
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('especie', 'area','anio')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)