#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 389
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



clean_sheet <- function(sheet_name) {
  
    print(paste("Procesando hoja:", sheet_name))
    
    raw <- readxl::read_excel(argendataR::get_raw_path(fuente_raw),
                              sheet = sheet_name,
                              col_names = F)
    
    columna_1 <-  raw %>% select(1) %>% 
      pull()
    
    fila_columna <- which(columna_1=="Año")
    
    skip <- fila_columna+1
    
    cols_ <- raw %>% slice(fila_columna)
    
    cols <- cols_[!white_cols(cols_)] %>%
      t() %>% # Transponer
      as.data.frame() %>%
      fill(V1, .direction = "down")
    
    cols$concatenado <- apply(cols, 1, function(x) {
      paste(stats::na.omit(x), collapse = "#")
    })
    
    # Leo datos
    sheet_data <- readxl::read_excel(get_raw_path(fuente_raw),
                                     sheet = sheet_name,
                                     skip = skip,
                                     col_names = F
    )
    
    sheet_data <- sheet_data[!white_cols(sheet_data)]
    
    cols <- cols$concatenado
    
    names(sheet_data) <- cols
    
    # cuento cantidad de columnas
    num_cols <- length(sheet_data)
    
    # saco las filas que tienen (num_cols - 1) nulos
    filter_bool <- check_na_threshold(sheet_data, num_cols - 1)
    
    # pivoteo datos y genero columna con nombre de provincia
    df <- sheet_data %>%
      dplyr::filter(!filter_bool) %>%
      pivot_longer(
        cols = !matches("Año"),
        names_to = "sexo",
        values_to = "poblacion_proyectada",
        values_transform = as.numeric
      ) %>%
      mutate(
        sheet_name = sheet_name) %>%
      janitor::clean_names()
    
    return(df)
  
}

# Obtener las hojas del archivo
sheets <- argendataR::get_raw_path(fuente_raw) %>%
  readxl::excel_sheets() %>%
  purrr::keep(~ all(grepl("^\\d+-.*", .x) & .x != "01-TOTAL DEL PAÍS"))

# Aplicar la función a cada hoja
df_raw <- map_dfr(sheets, clean_sheet) 

df_clean <- df_raw %>% 
  separate(sheet_name, into = c("provincia_id", "provincia"), sep = "-", extra = "merge") %>% 
  mutate(provincia_id = as.integer(provincia_id),
         provincia = ifelse(provincia == "SANTE FE", "SANTA FE", provincia)) %>% 
  rename(anio = ano)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Se quita la observación de total país dado que se deduce de la suma agregada de provincias")



id_fuente_clean <- 242
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  rename(anio = ano)


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'sexo', 'provincia_id')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
