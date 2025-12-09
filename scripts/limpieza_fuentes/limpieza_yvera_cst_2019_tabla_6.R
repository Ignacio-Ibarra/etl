#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 493
fuente_raw <- sprintf("R%sC0",id_fuente)

# Función para verificar si el número de NAs en cada fila es mayor o igual a un umbral
check_na_threshold <- function(df, threshold) {
  apply(df, 1, function(row) {
    sum(is.na(row)) >= threshold
  })
}

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}



clean_sheet_range <- function(sheet_name, filas_columnas, data_range, names_to, values_to){
  
  
  cols_ <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                              sheet = sheet_name,
                              col_names = F) %>% 
    slice(filas_columnas) %>% 
    select(-last_col())
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1:V3)
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  # Leo datos
  sheet_data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   range = data_range, 
                                   col_names = F, 
                                   na = "X")
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols$concatenado
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  df <- sheet_data %>% dplyr::filter(!filter_bool) %>% 
    pivot_longer(!all_of(cols$concatenado[1]),
                 names_to = names_to,
                 names_sep = "#",
                 values_to = values_to,
                 values_transform = as.numeric) %>% 
    janitor::clean_names()
  
  
  return(df)
}


sheet_name <- "Tabla 6" 
filas_columnas <- c(8,10,12)
data_range <- 'A15:AT28'
names_to <- c("categoria","sector","variable")
values_to <- 'valor'

df_clean <- clean_sheet_range(sheet_name = sheet_name, 
                               filas_columnas = filas_columnas, 
                               data_range = data_range, 
                               names_to = names_to,
                               values_to = values_to)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Cuadro: {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)


id_fuente_clean <- 316
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('productos', names_to)
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)