#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 310
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



clean_tabla <- function(sheet_name, filas_columnas, skip, names_to, values_to){
  
  
  cols_ <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                              sheet = sheet_name, 
                              col_names = F) %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  
  cols <- c('descripcion','producto', cols$concatenado[3:nrow(cols)])
  
  # Leo datos
  sheet_data <- readxl::read_excel(get_raw_path(fuente_raw), 
                                   sheet = sheet_name,
                                   skip = skip, 
                                   col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)] %>% select(-120) # viene con un error en esa columna
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1) #Con 4 nulos saco el footer
  df <- sheet_data %>% dplyr::filter(!filter_bool) %>%
    pivot_longer(!all_of(c('descripcion','producto')),
                 names_sep = "#",
                 names_to = names_to,
                 values_to = values_to,
                 values_transform = as.numeric) 
  
  return(df)
  
}


sheet_name <- readxl::excel_sheets(argendataR::get_raw_path(fuente_raw)) %>% 
  purrr::keep(., ~ grepl("mat.*ut.*pc.*",.,ignore.case = TRUE))
filas_columnas <- 4:5
skip <- 5
names_to <- c("nombre_sector", "codigo_sector")
values_to <- "valor"


df_clean <- clean_tabla(sheet_name = sheet_name,
                        filas_columnas = filas_columnas,
                        skip = skip, 
                        names_to = names_to,
                        values_to = values_to) 

sheet_name_normalized <- sheet_name %>% janitor::make_clean_names()

clean_filename <- glue::glue("{nombre_archivo_raw}_{sheet_name_normalized}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Cuadro: {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name
#                      )

id_fuente_clean <- 179
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("descripcion", "nombre_sector")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)