#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 439
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
  
  
  raw <- readxl::read_excel(get_raw_path(fuente_raw), 
                            sheet = sheet_name,  
                            col_names = F)
  
  idx <- which(raw$...1 == "Edad")
  
  hoja_list <- list()
  
  for (i in seq_along(idx)) {
    start_idx <- idx[i]
    if (i < length(idx)) {
      end_idx <- idx[i+1] - 1
    } else {
      end_idx <- nrow(raw)
    }
    
    raw_i <- raw %>% slice(start_idx:end_idx)
    
    cols_ <- raw_i %>% slice(1:2)
    
    cols <- cols_[!white_cols(cols_)] %>%
      t() %>% # Transponer
      as.data.frame() %>% 
      fill(V1, .direction = "down")
    
    cols$concatenado <- apply(cols, 1, function(x) {
      paste(stats::na.omit(x), collapse = "#")
    })
    
    sheet_data <- raw_i %>% slice_tail(n = -5)
    
    sheet_data <- sheet_data[!white_cols(sheet_data)]
    
    names(sheet_data) <- cols$concatenado
    
    num_cols <- length(sheet_data)
    
    filter_bool <- check_na_threshold(sheet_data, num_cols - 1)
    
    df <- sheet_data %>%
      dplyr::filter(!filter_bool) %>%
      pivot_longer(
        cols = !matches("Edad"),
        names_to = c("anio","sexo"),
        names_sep = "#",
        values_to = "poblacion_proyectada",
        values_transform = as.numeric
      ) %>% 
      mutate(anio = as.integer(anio))
    
    hoja_list[[i]] <- df
  }
  
  
  data <- bind_rows(hoja_list) %>% 
    mutate(sheet_name = sheet_name)
  
  return(data)
  
}

# Obtener las hojas del archivo
sheets <- argendataR::get_raw_path(fuente_raw) %>%
  readxl::excel_sheets() %>%
  purrr::keep(~ all(grepl("^\\d+-.*", .x) & .x != "01-TOTAL DEL PAÍS"))

# Aplicar la función a cada hoja
df_raw <- map_dfr(sheets, clean_sheet) 

diccionario_prov <- argendataR::get_raw_path("R84C0") %>% 
  read.csv() %>% 
  distinct(prov_cod, provincia = prov_desc)


df_clean <- df_raw %>% 
  janitor::clean_names() %>%  
  separate(sheet_name, into = c("provincia_id", "provincia"), sep = "-", extra = "merge") %>% 
  mutate(provincia_id = as.integer(provincia_id)) %>% 
  select(-provincia) %>% 
  left_join(diccionario_prov, join_by(provincia_id == prov_cod)) %>% 
  select(provincia_id, provincia, anio, edad, sexo, poblacion_proyectada)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Se quita la observación de total país dado que se deduce de la suma agregada de provincias")



id_fuente_clean <- 284
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  rename(anio = ano)


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'sexo', 'provincia_id', 'edad')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)