#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(tabulapdf)

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 341
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# Funciones para poder extraer tabla

find_header <- function(lineas, pattern){
  
  # Encontrar el header
  header_idx <- grep(pattern, lineas)
  if (length(header_idx) == 0) {
    stop("No se encontró el header en la string")
  }
  
  header <- unlist(strsplit(lineas[header_idx], "\t"))
  
  
  result = list(header_idx = header_idx, header = header)
  return(result)
  
}

es_linea_ok <- function(linea, len){
  
  elementos = unlist(strsplit(linea, "\t"))
  
  cond1 <- str_detect(elementos[1], "\\w+")
  
  cond2 <- sum(is.na(elementos)|elementos == "") == 0
  
  cond3 <- length(elementos) == len
  
  return(cond1 && cond2 && cond3)
  
}


reordenar_fila_no_ok <- function(linea, len){
  
  elementos = unlist(strsplit(linea, "\t"))
  
  curr_length = length(elementos)
  
  cat("Largo linea: ", curr_length, "\n\n")
  
  elementos <- c(elementos, rep("", len - curr_length))
  
  return(elementos)
}


procesar_string_larga <- function(string_larga, vector_completa_cadena, header = NULL, header_pattern = NULL) {
  # Separar en líneas
  lineas <- unlist(strsplit(string_larga, "\n"))
  
  if(is.null(header)){
  
  header_search = find_header(lineas, pattern = header_pattern)
  
  header_idx <- header_search$header_idx
  
  header <- header_search$header
  
  # Quitar el header de la string larga
  lineas <- lineas[-header_idx]
  
  }
  
  
  num_cols <- length(header)
  
  
  
  # Me quedo con las lineas que estan bien
  lineas_ok = lineas %>% 
    purrr::keep(., ~all(es_linea_ok(.x, len = num_cols))) %>% 
    map(~ strsplit(.x, "\t")[[1]]) %>% 
    map(~ tibble::as_tibble_row(setNames(.x, header))) %>% 
    bind_rows() %>% 
    janitor::clean_names()
  
  
  
  # Me quedo con las lineas que están mal. 
  lineas_no_ok <- lineas %>% 
    purrr::keep(., ~all(!es_linea_ok(.x, len = num_cols))) %>% 
    purrr::map(., ~reordenar_fila_no_ok(.x, len = num_cols)) %>% 
    do.call(rbind, .) %>% 
    as.data.frame(.) %>%
    setNames(header) %>%
    janitor::clean_names() %>% 
    mutate(cadena = vector_completa_cadena) %>% 
    group_by(cadena) %>% 
    summarise(across(everything(), ~ paste(.x, collapse = ""))) %>% 
    ungroup()
  
  if(nrow(lineas_no_ok) == 0){
    lineas_no_ok <- data.frame()
  }
    
    
  clean_df <- bind_rows(lineas_ok, lineas_no_ok)
      

  return(clean_df)
}



# Ruta al archivo PDF
pdf_path <- argendataR::get_raw_path(fuente_raw)

# Extraer tablas de las páginas 29 y 30
tables <- tabulapdf::extract_tables(pdf_path, pages = c(29,30), output = "character")


# Resultado para la pagina 29
string_larga_p29 <- tables[[1]]
patron_p29 <- "^CADENA(\\t\\d{4})+"
vector_completa_cadena_p29 <- rep(c("Bovino", "Lácteo", "Peras y Manzanas"), each = 3)
resultado_p29 <- procesar_string_larga(string_larga_p29, vector_completa_cadena = vector_completa_cadena_p29, header_pattern = patron_p29)


# Resultado para la pagina 30 (sin header)
string_larga_p30 <- tables[[2]]
header_p30 <- names(resultado_p29)
vector_completa_cadena_p30 <- rep(c("Soja", "Total CAA"), each = 3)
resultado_p30 <- procesar_string_larga(string_larga_p30, header = header_p30, vector_completa_cadena = vector_completa_cadena_p30)


df_clean <- bind_rows(resultado_p29, resultado_p30) %>% 
  dplyr::filter(!grepl("Total.*", cadena)) %>% 
  mutate(cadena = tools::toTitleCase(cadena)) %>% 
  setNames(., c("cadena",2001:2021)) %>% 
  mutate(across(as.character(2001:2021), ~as.numeric(str_remove(.x, "\\.")))) %>% 
  pivot_longer(!cadena, names_to = 'anio', 
               values_to = 'vab') %>% 
  mutate(unidad_medida = "precios constantes de 2007, en millones de pesos")


clean_filename <- glue::glue("{nombre_archivo_raw}_cuadro6_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - Anexo Cuadros - Cuadro 6 (Valor Agregado Bruto a precios constantes de 2007 por año, según CAA (en millones de pesos)")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

 
# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Se extrajo el cuadro 6 de las páginas 29 y 30 del paper fuente")



id_fuente_clean <- 214
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('cadena', 'anio')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)