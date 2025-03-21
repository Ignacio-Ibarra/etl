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

corregir_fila <- function(string_fila){
  
  string_fila <- str_replace_all(string_fila, "\t\t","\t")
  
  return(string_fila)
  
}

procesar_string_larga <- function(string_larga, vector_completa_cadena, header = NULL, header_idx = NULL) {
  # Separar en líneas
  lineas <- unlist(strsplit(string_larga, "\n"))
  
  # Quitar el header de la string larga
  lineas <- lineas[-header_idx]

  num_cols <- length(header)
  
  
  # Me quedo con las lineas que estan bien
  lineas_corregidas = lineas %>% 
    purrr::map(~corregir_fila(.x)[[1]]) %>% 
    map(~ strsplit(.x, "\\s(?=\\d)", perl = TRUE)[[1]]) %>% 
    map(~ tibble::as_tibble_row(setNames(.x, header))) %>% 
    bind_rows() 
  
  
 return(lineas_corregidas)
}



# Ruta al archivo PDF
pdf_path <- argendataR::get_raw_path(fuente_raw)

# Extraer tablas de la p35
table <- tabulapdf::extract_tables(pdf_path, pages = c(25), output = "character")


# Resultado para la pagina 31
string_larga_p25 <- table[[1]]
header_p25 <- c("cadena", "vbp_Mpcor","vbp_porc","expo_Mpcor","expo_porc","expo_vbp_ratio")
resultado_p25 <- procesar_string_larga(string_larga_p25, header = header_p25, header_idx = 1:3)




df_clean <- resultado_p25 %>% 
  mutate(vbp_Mpcor = as.numeric(str_replace_all(vbp_Mpcor, ",","")),
         vbp_porc = as.numeric(str_replace_all(vbp_porc, "%","")),
         expo_Mpcor = as.numeric(str_replace_all(expo_Mpcor, ",","")),
         expo_porc = as.numeric(str_replace_all(expo_porc, "%","")),
         expo_vbp_ratio = as.numeric(str_replace_all(expo_vbp_ratio, "%",""))
         ) %>% 
  dplyr::filter(cadena != "Total")


clean_filename <- glue::glue("{nombre_archivo_raw}_cuadro4_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - Anexo Cuadros - Cuadro 4 (Estructura de las CAA en el VBP y exportaciones (en millones de pesos corrientes). Año 2021")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Se extrajo el cuadro 4 de la página 25 del paper fuente")



id_fuente_clean <- 216
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('cadena')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)