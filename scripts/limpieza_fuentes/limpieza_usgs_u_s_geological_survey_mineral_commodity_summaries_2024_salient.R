#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


# Función para obtener la ruta del archivo, compatible tanto en RStudio como en la consola
get_file_location <- function() {
  # Intenta obtener la ruta del archivo en RStudio
  if (interactive() && "rstudioapi" %in% rownames(installed.packages())) {
    return(rstudioapi::getSourceEditorContext()$path)
  }
  
  # Alternativa para obtener la ruta si se usa source()
  this_file <- (function() { attr(body(sys.function(1)), "srcfile") })()
  
  # Si no se obtiene el path (e.g., en consola sin RStudio), asigna un valor por defecto
  if (!is.null(this_file)) {
    return(this_file$filename)
  } else {
    return("Archivo no especificado o ruta predeterminada")
  }
}

code_name <- get_file_location() %>% str_split_1(., "/") %>% tail(.,1)

id_fuente <- 267
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
  dplyr::filter(grepl(".*salient\\.csv$", Name)) %>% 
  mutate(basename = basename(Name))

archivos_xml <- archivos %>%
  dplyr::filter(grepl(".*meta\\.xml$", Name)) %>% 
  mutate(basename = basename(Name))

archivos_extraer <- c(archivos_csv$Name, archivos_xml$Name)

# Extraer solo los archivos filtrados
unzip(zip_path, files = archivos_extraer, exdir = tempdir(), junkpaths = T)


procesar_archivos_csv <- function(archivos_filtrados) {
  lista_dataframes <- list()
  archivos_con_errores <- c()  # Almacenar nombres de archivos con errores
  
  for (archivo in archivos_filtrados$basename) {
    # message("Procesando archivo: ", archivo)
    
    id <- str_extract(archivo, ".*-(.*)_salient", group = 1)
    tryCatch({
      
      data <- read.csv(file.path(tempdir(), archivo)) %>% janitor::clean_names()
      
    }, error = function(e) {
      message("Error al leer el archivo: ", archivo, " - ", e$message)
      archivos_con_errores <- c(archivos_con_errores, archivo)
      return(NULL)
    })
    
    if (is.null(data) || nrow(data) == 0 || ncol(data) == 0) {
      message("Archivo ", archivo, " está vacío o no contiene datos válidos.")
      archivos_con_errores <- c(archivos_con_errores, archivo)
      next
    }
    
    # message("Columnas del archivo antes del pivot_longer: ", paste(names(data), collapse = ", "))
    
    data <- data[rowSums(is.na(data)) < ncol(data), ]
    data <- data[, colSums(is.na(data)) < nrow(data)]
    data$id <- id
    data <- data[, names(data) != "X"]
    data[] <- lapply(data, as.character)
    
    errores <- list()
    pivoted_data <- tryCatch({
      data %>% pivot_longer(
        !all_of(c("data_source", "commodity", "year", "id")),
        names_to = 'variable', values_to = 'value'
      )
    }, error = function(e) {
      errores <- append(errores, list(paste("Error con 'commodity':", e$message)))
      NULL
    })
    
    if (is.null(pivoted_data)) {
      pivoted_data <-   tryCatch({
        data %>% pivot_longer(
          !all_of(c("data_source", "country", "year", "id")),
          names_to = 'variable', values_to = 'value'
        )
      }, error = function(e) {
        errores <- append(errores, list(paste("Error con 'Country':", e$message)))
        NULL
      })
    }
    
    if (is.null(pivoted_data)) {
      pivoted_data <-   tryCatch({
        data %>% pivot_longer(
          !all_of(c("source", "country", "type", "id")),
          names_to = 'variable', values_to = 'value'
        )
      }, error = function(e) {
        errores <- append(errores, list(paste("Error con 'Source' y 'Type':", e$message)))
        NULL
      })
    }
    
    if (is.null(pivoted_data)) {
      pivoted_data <-   tryCatch({
        data %>% pivot_longer(
          !all_of(c("data_source", "country", "type", "id")),
          names_to = 'variable', values_to = 'value'
        )
      }, error = function(e) {
        errores <- append(errores, list(paste("Error con 'Source' y 'Type':", e$message)))
        NULL
      })
    }
    
    if (is.null(pivoted_data) && length(errores) > 0) {
      message("No se pudo procesar el archivo: ", archivo)
      for (error in errores) {
        message(error)
      }
      archivos_con_errores <- c(archivos_con_errores, archivo)
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
df_dataframes <- bind_rows(lista_dataframes) %>% 
  janitor::clean_names() %>% 
  mutate(data_source = ifelse(!is.na(source), source, data_source),
         commodity = ifelse(!is.na(type), type, commodity)) %>% 
  select(-c(source,type)) %>% 
  mutate(variable = case_when( # acá pongo todas las correcciones a mano que necesite
    id == "mercu" & variable == "price_imports_df" ~ "price_imports_dkg", 
    TRUE ~ variable)
    ) %>% 
  mutate(variable_clean = gsub("_","", variable))

df_tablas <- bind_rows(lista_tablas) %>% 
  mutate(label_clean = gsub("_","", label))

df_clean <- df_dataframes %>% 
  left_join(df_tablas, join_by(variable_clean == label_clean, id)) %>% 
  select(-label, -variable_clean)


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
#                      descripcion = glue::glue("El dataset contiene una compilación de todos los archivos contenidos en salient.zip, se puede buscar los datos en la tabla filtrando por id. La lista de ids es {lista_metales}"))

id_fuente_clean <- 135
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("id", "year", "variable")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)