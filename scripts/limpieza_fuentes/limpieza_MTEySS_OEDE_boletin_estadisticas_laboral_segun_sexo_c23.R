# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 235
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
                   'Hogares privados con servicio doméstico', 'Sin sector definido')

letra <-  c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Z')

letra_desc_abrev <-  c('Agro', 'Pesca', 'Petróleo y minería', 'Industria manufacturera', 
                       'Electricidad, gas y agua', 'Construcción', 'Comercio', 'Hotelería y restaurantes', 
                       'Transporte y comunicaciones', 'Finanzas', 'Serv. inmobiliarios y profesionales', 
                       'Adm. pública y defensa', 'Enseñanza', 'Salud', 'Serv. comunitarios, sociales y personales', 'Servicio doméstico','No definido')

#añado descripción de letra de indec 
dicc_sector <- data.frame(letra, sector_letra, letra_desc_abrev)




clean_cuadros <- function(sheet_name, skip, filas_columnas, names_to, values_to){
  
  str_titulos <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                                     sheet = sheet_name,
                                     range = "A1:A3",
                                     col_names = F)
  
  seccion_str <- str_titulos$...1[1]
  cuadro_str <- str_titulos$...1[2]
  detalle_str <- str_titulos$...1[3]
  
  
  
  cols_ <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                              sheet = sheet_name,
                              col_names = F) %>% slice(filas_columnas)
  
  cols <- cols_[!white_cols(cols_)] %>%
    t() %>% # Transponer
    as.data.frame() %>% 
    fill(V1, .direction = "down")
  
  cols$concatenado <- apply(cols, 1, function(x) {
    paste(stats::na.omit(x), collapse = "#")
  })
  
  cols <- cols %>% 
    pull(concatenado) %>% gsub("sd","Z", .)
  
  # Leo datos
  sheet_data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), 
                                   sheet = sheet_name, 
                                   skip = skip, 
                                   col_names = F)
  
  sheet_data <- sheet_data[!white_cols(sheet_data)]
  
  names(sheet_data) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(sheet_data)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(sheet_data, num_cols-1)
  data <- sheet_data %>% dplyr::filter(!filter_bool)
  
  # pivoteo datos y genero columna con nombre de provincia
  df <- data %>% pivot_longer(cols = -all_of("Período (trimestre año)"), 
                              names_to = names_to,
                              names_sep = "#",
                              values_to = values_to, 
                              values_transform = as.numeric) %>% 
    select(-letra_desc) %>% 
    left_join(dicc_sector, join_by(letra)) %>% 
    mutate(seccion = seccion_str, 
           cuadro = cuadro_str,
           detalle = detalle_str) %>% 
    janitor::clean_names()
   
  
  return(df)
}


sheet_name <- "C 2.3"
skip <- 6
filas_columnas <- 4:6
names_to <- c('letra_desc','letra','sexo')
values_to <- 'puestos'

df_clean <- clean_cuadros(sheet_name = sheet_name, skip = skip, filas_columnas = filas_columnas, names_to = names_to, values_to = values_to )



# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Cuadro: {sheet_name}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 105
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(sexo = ifelse(grepl("Muj", sexo), "Mujeres", "Varones"))

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('periodo_trimestre_ano', 'letra', 'sexo')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)


