code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 264
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


zip_path <- argendataR::get_raw_path(fuente_raw)

# Listar los archivos dentro del ZIP
archivos <- unzip(zip_path, list = TRUE)

# Filtrar archivos

archivos_csv <- archivos %>%
  dplyr::filter(grepl(".*world\\.csv$", Name)) %>% 
  mutate(basename = basename(Name))

archivos_xml <- archivos %>%
  dplyr::filter(grepl(".*meta\\.xml$", Name)) %>% 
  mutate(basename = basename(Name))

archivos_extraer <- c(archivos_csv$Name, archivos_xml$Name)

# Extraer solo los archivos filtrados
unzip(zip_path, files = archivos_extraer, exdir = tempdir(), junkpaths = T)


procesar_archivos_csv <- function(archivos_filtrados) {
  
  lista_dataframes <- list()
  archivos_con_errores <- c()
  
  for (archivo in archivos_filtrados$basename) {

  
  # Extraer el substring del nombre del archivo (por ejemplo, antes del ".csv")
  id <- str_extract(archivo, ".*-(.*)_world", group = 1 )
  
  # Leer el archivo CSV en un data.frame
  data <- read.csv(file.path(tempdir(), archivo))
  
  # Eliminar filas completamente nulas
  data <- data[rowSums(is.na(data)) < ncol(data), ]
  
  # Eliminar columnas completamente nulas
  data <- data[, colSums(is.na(data)) < nrow(data)]
  
  # Agregar la columna "id" con el substring extraído
  data$id <- id
  
  cols <- names(data)
  
  data <- data[, cols[cols!="X"]]
  
  data[] <- lapply(data, as.character)
  
  data <- data %>% janitor::clean_names() 
  
  unpivot_possible_cols <- c("data_source", "source", "type", "year", "country", "commodity", "id")
  
  unpivot_cols <- unpivot_possible_cols[unpivot_possible_cols %in% names(data)]
  
  errores <- list()
  
  pivoted_data <- tryCatch({
    data %>% pivot_longer(
      !all_of(unpivot_cols),
      names_to = 'variable', values_to = 'value'
    )
  }, error = function(e) {
    errores <- append(errores, list(e$message))
    NULL
  })
  
  
  if (is.null(pivoted_data)) {
    
    if(length(errores) > 0){
    message("No se pudo procesar el archivo: ", archivo)
      for (error in errores) {
        message(error)
      }
      archivos_con_errores <- c(archivos_con_errores, archivo)
    }else{
      message("por alguna razon no se guardaron los errores")
    }
    
  } else {
    lista_dataframes[[id]] <- pivoted_data %>% mutate(value = ifelse(value == "", NA, value)) %>% drop_na(value)
  }
  }
  
  message("Cantidad de archivos con errores: ", length(archivos_con_errores))
  message("Cantidad de archivos procesados exitosamente: ", length(lista_dataframes))
  message("Archivos con errores: ", paste(archivos_con_errores, collapse = ", "))
  
  return(lista_dataframes)
}


# Función para parsear los atributos de datos del archivo XML
parse_xml_attributes <- function(xml_data) {
  attributes <- xml2::xml_find_all(xml_data, "//eainfo/detailed/attr") %>%
    lapply(function(attr) {
      data.frame(
        Label = xml2::xml_text(xml2::xml_find_first(attr, "attrlabl")),
        Definition = xml2::xml_text(xml2::xml_find_first(attr, "attrdef")),
        Domain = xml2::xml_text(xml2::xml_find_first(attr, "attrdomv/edom/edomv")),
        DomainDescription = xml2::xml_text(xml2::xml_find_first(attr, "attrdomv/edom/edomvd")),
        Unit = xml2::xml_text(xml2::xml_find_first(attr, "attrdomv/rdom/attrunit")),
        stringsAsFactors = FALSE
      )
    }) %>%
    bind_rows()
  return(attributes)
}


procesar_archivos_xml <- function(archivos_filtrados){
  
  lista_tablas <- list()
  
  for (archivo in archivos_filtrados$basename) {
    
    # message("Procesando archivo: ", archivo)
    
    id <- str_extract(archivo, ".*-(.*)_meta", group = 1)
    
    id <- ifelse(id == "alumi", "alum", id)
    
    xml_path <- file.path(tempdir(), archivo)
    
    xml_content <- readLines(xml_path, warn = FALSE)
    
    # Reemplazar caracteres & no escapados
    xml_content <- gsub("&(?!amp;|lt;|gt;|apos;|quot;)", "&amp;", xml_content, perl = TRUE)
    
    # Guardar el contenido corregido en una variable
    xml_data <- xml2::read_xml(paste(xml_content, collapse = "\n"))
    
    attributes_table <- parse_xml_attributes(xml_data) %>% 
      mutate(id = id,
             Label = janitor::make_clean_names(Label)) %>% 
      janitor::clean_names() %>% 
      select(id, label, definition, unit)
    
    lista_tablas[[id]] <- attributes_table
    
  }
  
  return(lista_tablas)
  
}


lista_dataframes <- procesar_archivos_csv(archivos_csv)


lista_tablas <- procesar_archivos_xml(archivos_xml)


# Luego, combinar los dataframes
df_dataframes <- bind_rows(lista_dataframes) 

df_tablas <- bind_rows(lista_tablas)

df_clean <- df_dataframes %>% 
  left_join(df_tablas, join_by(variable == label, id))

lista_metales <- unique(df_clean$id)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Compilación de todos los minerales")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = glue::glue("El dataset contiene una compilación de todos los archivos contenidos en world.zip, se puede buscar los datos en la tabla filtrando por id. La lista de ids es {lista_metales}"))

id_fuente_clean <- 134
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("id", "variable", "country")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
