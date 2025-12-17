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


clean_table <- function(X, filas_columnas = 3, filas_datos = 5:12, primeras_n_cols = 5, names_to = "indicador", values_to = "value"){
  
  
  cols_ <- X %>% slice(filas_columnas) %>% 
    select((1:primeras_n_cols))
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1)
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
   cols_clean <- cols$concatenado %>%
    { .[-1] } %>% 
    c("industria_turistica", .)
  
  anio_tabla <- X %>% 
    slice(1) %>% 
    select(1) %>% 
    pull() %>% 
    str_extract(., "\\d{4}") %>% 
    as.integer()
  
  # Leo datos
  sheet_data <- X %>% slice(filas_datos) %>% 
    select(1:primeras_n_cols)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols_clean
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  df <- sheet_data %>% dplyr::filter(!filter_bool) %>% 
    pivot_longer(!all_of("industria_turistica"),
                 names_to = names_to,
                 values_to = values_to,
                 values_transform = as.numeric) %>% 
    mutate(anio = anio_tabla) 
  
  
  return(df)
}


clean_sheet <- function(raw, idxs){
  
  results <- data.frame()
  for (i in 1:length(idxs)){
    
    start_row = idxs[i]
    
    if (i == length(idxs)){
      end_row = nrow(raw)
    }else{
      end_row = idxs[i+1] - 3
    }
    
    raw_data <- raw %>% 
      slice(., start_row:end_row) 
    
    results <- results %>% 
      bind_rows(.,
                clean_table(X = raw_data)
      )
  }
  
  return(results)
}


sheet_name <- "Empleo en las IT" 
df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  readxl::read_excel(., sheet = sheet_name, 
                     col_names = F)


idxs <- which(grepl("Año", df_raw$...1))


df_clean <- clean_sheet(df_raw, idxs) %>% 
  dplyr::filter(industria_turistica != "Industrias turísticas / Total Economía")



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


id_fuente_clean <- 317
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'indicador', 'industria_turistica')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)