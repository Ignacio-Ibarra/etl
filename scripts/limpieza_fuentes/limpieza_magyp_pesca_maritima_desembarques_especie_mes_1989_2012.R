#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 329
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





clean_sheet <- function(worksheet, sheet_name, skip, filas_columnas, names_to, values_to, anio){
  
  sheet <- worksheet[[sheet_name]]
  
  cols_ <- sheet %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  
  # Leo datos
  sheet_data <- sheet %>% dplyr::filter(1:nrow(sheet)>skip)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  cols <- c("especie", cols$concatenado[-1], "grupo")
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  
  # pivoteo datos y genero columna con nombre de provincia
  df <- sheet_data %>% 
    dplyr::filter(!filter_bool) %>% 
    mutate(grupo = ifelse(especie == "Merluza hubbsi sur 41º", "PEZ", grupo)) %>% 
    dplyr::filter(!is.na(grupo)) %>% 
    pivot_longer(cols = !c(especie, grupo), 
                 names_to = names_to,
                 values_to = values_to, 
                 values_transform = as.numeric) %>% 
    mutate(anio = anio
    ) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!grepl("Total.*", mes, ignore.case = T)) %>% 
    select(anio, mes, grupo, especie, desembarque_toneladas)
  
  
  return(df)
  
}

clean_sheet_2012 <- function(sheet, skip, filas_columnas, names_to, values_to, anio){
  
  cols_ <- sheet %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  
  # Leo datos
  sheet_data <- sheet %>% dplyr::filter(1:nrow(sheet)>skip)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  cols <- c("especie", cols$concatenado[-1])
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  
  
  # pivoteo datos y genero columna con nombre de provincia
  df <- sheet_data %>% 
    dplyr::filter(!filter_bool) %>%
    pivot_longer(cols = !especie, 
                 names_to = names_to,
                 values_to = values_to, 
                 values_transform = as.numeric) %>% 
    mutate(anio = anio
    ) %>% 
    janitor::clean_names() %>% 
    dplyr::filter(!grepl("Total.*", mes, ignore.case = T)) %>% 
    dplyr::filter(!grepl(".*total", especie, ignore.case = T)) %>% 
    select(anio, mes, especie, desembarque_toneladas)
  
  
  return(df)
  
}


clean_worksheet <- function(lista_data, inner_file, skip, filas_columnas, names_to, values_to){
  
  anio <- str_extract(inner_file, "\\d{4}") %>% as.integer()
  
  worksheet <- lista_data[[inner_file]]
  
  sheets <- names(worksheet)
  
  data_anual <- purrr::map_dfr(sheets, function(sheet_name){ 
    clean_sheet(worksheet = worksheet,
                sheet_name = sheet_name,
                skip = skip, 
                filas_columnas = filas_columnas,
                names_to = names_to,
                values_to = values_to,
                anio = anio)
  }
  ) 
  
  return(data_anual)
  
}


json_data_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(., simplifyVector = TRUE)


inner_files <- names(json_data_raw)[1:length(json_data_raw)-1]

df_stage_hasta_2011 <- purrr::map_dfr(inner_files, function(inner_file){
  clean_worksheet(lista_data = json_data_raw,
                  inner_file = inner_file,
                  skip = 3,
                  filas_columnas = 3,
                  names_to = "mes",
                  values_to = "desembarque_toneladas")
}
) %>% drop_na(desembarque_toneladas)


diccionario_grupo_especie <- df_stage_hasta_2011 %>% distinct(grupo, especie)

df_stage_2012 <- clean_sheet_2012(sheet = json_data_raw[['inner_file_2012.xls']][['esp_mes_2012']],
                                  filas_columnas = 3, skip = 3,
                                  names_to = "mes",
                                  values_to = "desembarque_toneladas",
                                  anio = 2012) %>% 
  left_join(diccionario_grupo_especie, join_by(especie)) %>% 
  mutate(
    grupo = case_when(
      grepl("Anchoíta al.*|Caballa al.*|Cojinova.*|Merluza hubbsi al.*|Tiburón pintarroja.*", especie) ~ "PEZ",
      grepl("Chucho Myliobatis.*", especie) ~ "OTROPEZ",
      TRUE ~ grupo
    )
  )



df_clean <- bind_rows(df_stage_hasta_2011, df_stage_2012) %>% 
  arrange(anio, grupo, especie)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 206
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'mes', 'especie')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
