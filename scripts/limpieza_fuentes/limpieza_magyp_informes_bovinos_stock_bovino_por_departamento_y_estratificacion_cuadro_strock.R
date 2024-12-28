# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 304
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



clean_cuadro_stock <- function(sheet_name, 
                               filas_columnas, 
                               title_range, 
                               skip, 
                               reemplazos,
                               names_to,
                               values_to){
  
  str_title <-  argendataR::get_raw_path(fuente_raw) %>% 
    readxl::read_excel(.,
                       sheet = sheet_name, 
                       range = title_range, 
                       col_names = F) %>% pull()
  
  cols_ <- argendataR::get_raw_path(fuente_raw) %>% 
    readxl::read_excel(.,
                       sheet = sheet_name,  
                       col_names = F) %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  
  sheet_data <- argendataR::get_raw_path(fuente_raw) %>% 
    readxl::read_excel(.,
                       sheet = sheet_name,  
                       skip = skip,
                       col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  cols_corregidas <- purrr::map_chr(cols$concatenado, function(x) ifelse(x %in% names(reemplazos), reemplazos[x], x))
    
  names(sheet_data) <- cols_corregidas
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  data <- sheet_data %>% dplyr::filter(!filter_bool)
  
  names_sep <- if (length(names_to) > 1) {
    "#"
  } else {
    NULL
  }
  
  # pivoteo datos y genero columna con nombre de provincia
  df <- data %>% pivot_longer(cols = -all_of(unname(reemplazos)), 
                              names_to = names_to,
                              names_sep = names_sep,
                              values_to = values_to, 
                              values_transform = as.numeric) 
  
  result <- list(title = str_title, data = df)
  
  return(result)
  
}

sheet_name = "Stock"
skip = 3
title_range = "B2"
names_to = "tipo_bovino"
values_to = "cantidad"
filas_columnas = 2

reemplazos <- c(
  "Año"  = "anio",
  "Provincia" = "provincia",
  "Provincia\n(sin tilde)" = "provincia_sin_tilde",
  "Partido/Departamento MINUSCULA" = "departamento",
  "Partido/Departamento \n(sin tilde)" = "departamento_sin_tilde"
)




result <- clean_cuadro_stock(sheet_name = sheet_name,
                             filas_columnas = filas_columnas,
                             title_range = title_range,
                             skip = skip,
                             reemplazos = reemplazos,
                             names_to = names_to,
                             values_to = values_to)
  
sheet_name_normalized <- sheet_name %>% janitor::make_clean_names(.)


clean_filename <- glue::glue("{nombre_archivo_raw}_cuadro_{sheet_name_normalized}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - {result$title}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

result$data %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 172
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = unname(reemplazos)
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)