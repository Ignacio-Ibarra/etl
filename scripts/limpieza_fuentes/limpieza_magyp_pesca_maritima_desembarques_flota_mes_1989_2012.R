#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 334
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


clean_sheet <- function(worksheet, sheet_name, anio,
                        skip, 
                        names_to, 
                        values_to){
  tryCatch({
    sheet <- worksheet[[sheet_name]]
    
    grupo_flota <- c("FRESQUEROS", "CONGELADORES")
    
    meses_es <- c('Enero', 'Febrero', 'Marzo', 'Abril', 'Mayo', 
                  'Junio', 'Julio', 'Agosto', 'Septiembre', 
                  'Octubre', 'Noviembre', 'Diciembre', 'Total')
    
    # Leo datos
    sheet_data <- sheet %>% dplyr::filter(1:nrow(sheet) > skip)
    
    sheet_data <- sheet_data[!white_cols(sheet_data)]
    
    cols <- c("flota", meses_es)
    
    names(sheet_data) <- cols
    
    # cuento cantidad de columnas
    num_cols <- length(sheet_data)
    
    # saco las filas que tienen (num_cols - 1) nulos
    filter_bool <- check_na_threshold(sheet_data, num_cols - 1)
    
    # pivoteo datos y genero columna con nombre de provincia
    df <- sheet_data %>% 
      mutate(across(all_of(meses_es), ~ ifelse(. %in% meses_es, NA, .))) %>% 
      mutate(grupo_flota = ifelse(flota %in% grupo_flota, flota, NA)) %>%
      tidyr::fill(grupo_flota, .direction = "down") %>%
      filter(!(flota %in% c("SUBTOTAL", "TOTAL"))) %>%
      filter(!if_all(all_of(meses_es), is.na)) %>% 
      mutate(anio = anio,
             flota = toupper(flota),
             grupo_flota = toupper(grupo_flota)) %>%
      pivot_longer(!all_of(c("flota","grupo_flota", "anio")), 
                   names_to = names_to, 
                   values_to = values_to,
                   values_transform = as.numeric) %>% 
      janitor::clean_names() %>% 
      filter(mes != "Total")
    
    return(df)
    
  }, error = function(e) {
    message(sprintf("Error en hoja '%s': %s", sheet_name, e$message))
    return(NULL)
  })
}



clean_worksheet <- function(lista_data, inner_file, clean_func){
  
  anio <- str_extract(inner_file, "\\d{4}") %>% as.integer()
  
  worksheet <- lista_data[[inner_file]]
  
  sheets <- names(worksheet)
  
  data_anual <- purrr::map_dfr(sheets, function(sheet_name){ 
    clean_func(worksheet = worksheet,
               sheet_name = sheet_name,
               anio = anio)
  }
  ) 
  
  return(data_anual)
  
}


json_data_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(., simplifyVector = TRUE)


all_files <- names(json_data_raw)


clean_sheet_particular <- function(worksheet, sheet_name, anio){
  clean_sheet(worksheet = worksheet,
              sheet_name = sheet_name,
              anio = anio,
              skip = 2,
              names_to = "mes",
              values_to = "desembarque_toneladas")
}

df_clean <- purrr::map_dfr(all_files, function(inner_file){
  clean_worksheet(lista_data = json_data_raw,
                  inner_file = inner_file,
                  clean_func = clean_sheet_particular)
})


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 209
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'mes', 'puertos')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)