#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 237
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




clean_cuadro_c5 <- function(sheet_name, skip, filas_columnas, names_to, values_to){
  
  str_titulos <- readxl::read_excel(get_raw_path(fuente_raw), 
                                    sheet = sheet_name,
                                    range = "A1:A1",
                                    col_names = F) %>% pull() %>% str_replace(., "Cuadro 4:","Cuadro 5:")
  
  cols_ <- readxl::read_excel(get_raw_path(fuente_raw), 
                              sheet = sheet_name,
                              col_names = F) %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() 
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  cols <- c('ciiu_rev3_4d',cols$concatenado)
  
  # Leo datos
  sheet_data <- readxl::read_excel(get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   skip = skip, 
                                   col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  df <- sheet_data %>% dplyr::filter(!filter_bool) %>% 
    pivot_longer(!all_of(cols[1:2]),
                 names_to = names_to,
                 values_to = values_to,
                 names_transform = as.integer,
                 values_transform = as.numeric) %>% 
    mutate(cuadro = str_titulos) %>% 
    janitor::clean_names() 
  
  
  return(df)
}


sheet_name <- "C 5" # Cuadro 4 dice en la celda A1:A1 pero es Cuadro 5
filas_columnas <- 4:5
skip <- 6
names_to <- 'anio'
values_to <- 'cant_empresas_privadas_activas'

df_clean <- clean_cuadro_c5(sheet_name = sheet_name, skip = skip, filas_columnas = filas_columnas, names_to = names_to, values_to = values_to )



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

actualizar_fuente_clean(id_fuente_clean = 108,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        descripcion = "El archivo contiene la cantidad de empresas privadas activas a nivel de 4 dígitos del CIIU Rev. 3, para poder anonimizar hay varios sectores donde no se presentan datos porque la cantidad de empresas es muy pequeña, es por ello que se conservan las fila donde se presenta el valor total de todas las empresas por anio, allí donde rama_de_actividad == 'Total'")




