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


id_fuente <- 36
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

# PBI en US$ --------------------------------------------------------------

sheet_name <-  "OyD %PIB, Precios corr. "

str_title <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = sheet_name, col_names = F, n_max = 1) %>%
  rowwise() %>% # Procesar fila por fila
  mutate(concatenado = paste(c_across(everything()), collapse = " - ")) %>%
  ungroup() %>% pull(concatenado)


data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw),
                           sheet = sheet_name, skip = 1)

data <- data %>% 
  rename(anio  = Año)

# filtro filas vacias
# fila 4 solo contiene un signo "%" 

data <- data[-4,] %>% 
  mutate(across(everything(), function(x) replace(x, x=="#DIV/0!", NA))) %>% 
  filter(!if_all(-anio, is.na)) 

# pivot longer los datos
data <- data %>% 
  mutate(across(everything(), function(x) replace(x, x=="...", NA))) %>%
  mutate(across(everything(), as.character)) %>% 
  pivot_longer(cols = -anio,
               names_to = "indicador", values_to = "valor")

data <- data %>% 
  mutate(valor = ifelse(as.numeric(anio) < 1935,
                        100*as.numeric(valor),
                        as.numeric(valor)))

# unidad
data <- data %>% 
  mutate(unidad = "% del PBI a precios corrientes")


# agrego columna de pertenencia geografica
data <- data %>% 
  mutate(iso3 = "ARG")

# seleccion y rename de columnas
data <- data %>% 
  select(anio, iso3, indicador, unidad, valor)


# valores a numerico
df_clean <- data %>% 
  mutate(valor = as.numeric(valor),
         anio = as.numeric(anio))

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names()

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


clean_title <- glue::glue("{titulo.raw} - {str_title}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 10
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "indicador", "unidad"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)