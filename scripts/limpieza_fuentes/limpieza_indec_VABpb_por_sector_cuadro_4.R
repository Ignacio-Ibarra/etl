#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 223
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

sector_letra <-  c('Agricultura, ganadería, caza y silvicultura', 'Pesca', 'Explotación de minas y canteras', 
                 'Industria manufacturera', 'Electricidad, gas y agua', 'Construcción', 
                 'Comercio mayorista, minorista y reparaciones', 'Hoteles y restaurantes', 
                 'Transporte, almacenamiento y comunicaciones', 'Intermediación financiera', 
                 'Actividades inmobiliarias, empresariales y de alquiler', 
                 'Administración pública y defensa; planes de seguridad social de afiliación obligatoria', 
                 'Enseñanza', 'Servicios sociales y de salud', 'Otras actividades de servicios comunitarias, sociales y personales', 
                 'Hogares privados con servicio doméstico')

letra <-  c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P')

letra_desc_abrev <-  c('Agro', 'Pesca', 'Petróleo y minería', 'Industria manufacturera', 
                     'Electricidad, gas y agua', 'Construcción', 'Comercio', 'Hotelería y restaurantes', 
                     'Transporte y comunicaciones', 'Finanzas', 'Serv. inmobiliarios y profesionales', 
                     'Adm. pública y defensa', 'Enseñanza', 'Salud', 'Serv. comunitarios, sociales y personales', 'Servicio doméstico')

#añado descripción de letra de indec 
dicc_sector <- data.frame(sector_letra, letra, letra_desc_abrev)


clean_cuadros_trimestrales <- function(sheet_name, skip){
  
  str_variable <- readxl::read_excel(get_raw_path(fuente_raw), 
                                     sheet = sheet_name,
                                     range = "A1:A2",
                                     col_names = F) %>% 
    pull() %>% paste0(., collapse = " - ")
                                     
  cols_ <- readxl::read_excel(get_raw_path(fuente_raw), 
                             sheet = sheet_name,
                             n_max = 5,
                             col_names = F) %>% tail(., 2) 
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>%
    fill(everything(), .direction = "down") %>% 
    mutate(concatenado = paste(V1, V2, sep="#")) %>% 
    pull(concatenado)
  
  # Leo datos
  sheet_data <- readxl::read_excel(get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   skip = skip, 
                                   col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- c("sub_sector",cols)
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  data <- sheet_data %>% dplyr::filter(!filter_bool)
  
    # pivoteo datos y genero columna con nombre de provincia
  df <- data %>% pivot_longer(cols = -all_of("sub_sector"), 
                              names_to =c('anio','trimestre'),
                              names_sep = "#",
                              values_to = 'vab_pb', 
                              values_transform = as.numeric) %>% 
    mutate(sector = ifelse(sub_sector %in% sector_letra, sub_sector, NA)) %>% 
    fill(sector) %>% 
    mutate(sub_sector = ifelse(sector == sub_sector, "Total sector", sub_sector),
           anio = as.numeric(str_extract(anio, "\\d{4}"))) %>% 
    left_join(dicc_sector, by = join_by(sector ==sector_letra)) %>% 
    select(anio, trimestre, letra, sector, letra_desc_abrev, sub_sector, vab_pb)
  
  return(df)
}


sheet_name <- "Cuadro 4"

skip <- 8

df_clean <- clean_cuadros_trimestrales(sheet_name = sheet_name, skip = skip )



# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 94
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "trimestre", "letra", "sub_sector")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)