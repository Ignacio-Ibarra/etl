#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 470
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



clean_sheet_range <- function(sheet_name, col_range, data_range, names_to, values_to){
  
  
  cols_ <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                              sheet = sheet_name,
                              range = col_range,
                              col_names = F) 
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() 
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  unidad_medida <- cols$concatenado %>% 
    { .[1]} %>% 
    str_extract(., "\\((.*)\\)", group=1)
  
  cols <- cols$concatenado %>% 
    str_remove_all(., "\\\r|\\\n|Año ") %>%
    { .[-1] } %>% 
    c("indicador", .)
  
  # Leo datos
  sheet_data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   range = data_range, 
                                   col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  df <- sheet_data %>% dplyr::filter(!filter_bool) %>% 
    pivot_longer(!all_of("indicador"),
                 names_to = names_to,
                 values_to = values_to,
                 names_transform = as.integer,
                 values_transform = as.numeric) %>% 
    mutate(unidad_medida = unidad_medida)
  
  
  return(df)
}


sheet_name <- "Resumen" 
col_range <- 'A7:I7'
data_range <- 'A9:I13'
names_to <- 'anio'
values_to <- 'valor'

df_stageA <- clean_sheet_range(sheet_name = sheet_name, 
                              col_range = col_range, 
                              data_range = data_range, 
                              names_to = names_to, 
                              values_to = values_to)

sheet_name <- "Resumen" 
col_range <- 'A16:I16'
data_range <- 'A18:I29'
names_to <- 'anio'
values_to <- 'valor'

df_stageB <- clean_sheet_range(sheet_name = sheet_name, 
                               col_range = col_range, 
                               data_range = data_range, 
                               names_to = names_to, 
                               values_to = values_to) %>% 
  mutate(unidad_medida = ifelse(indicador == "Puestos de trabajo en las industrias turísticas  (en miles)", "En miles de personas", unidad_medida))


df_clean <- bind_rows(df_stageA, df_stageB)


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


id_fuente_clean <- 308
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'indicador', 'unidad_medida')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)