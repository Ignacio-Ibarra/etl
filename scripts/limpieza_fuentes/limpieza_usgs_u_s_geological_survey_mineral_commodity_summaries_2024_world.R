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
archivos_filtrados <- archivos$Name[grepl(".*world\\.csv$", archivos$Name)]

# Extraer solo los archivos filtrados
unzip(zip_path, files = archivos_filtrados, exdir = tempdir())


# Crear una lista vacía para almacenar cada data.frame
lista_dataframes <- list()

# Iterar sobre el vector de archivos filtrados
for (archivo in archivos_filtrados) {
  
  # print(archivo)
  
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
  
  # Agregar el data.frame a la lista
  lista_dataframes[[length(lista_dataframes) + 1]] <- data
}

# Convertir todas las columnas a character antes de hacer bind_rows
lista_dataframes <- lapply(lista_dataframes, function(df) {
  df[] <- lapply(df, as.character)  # Convertir todas las columnas a character
  return(df)
})

# Luego, combinar los dataframes
df_clean <- bind_rows(lista_dataframes) %>% 
  janitor::clean_names() %>% 
  pivot_longer(!all_of(c("id", "source","country", "type")), 
                       names_to = 'variable', 
                       values_to = "value",
                       values_transform = as.character) %>%
  mutate(value = ifelse(value == "", NA, value)) %>% 
  drop_na(value)


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
                                     pk = c("cod_act", "destino_venta", "detalle")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
