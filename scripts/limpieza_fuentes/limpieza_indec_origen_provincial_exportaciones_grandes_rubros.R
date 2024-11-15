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


id_fuente <- 274
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


clean_opex_provincia_rubro <- function(sheet_name, skip, lista_provincias){
  
  
  
  df_clean <- readxl::read_excel(get_raw_path(fuente_raw),
                                 sheet = sheet_name, 
                                 col_names = F,
                                 skip = skip) 
  
  anios <- str_split_1(sheet_name, "-") %>% str_extract(., "(\\d{4}).*", group=1) %>%  as.integer(.)
  
  cols <- c("provincia_rubro",anios[1]:anios[2])
  
  names(df_clean) <- cols
  
  # cuento cantidad de columnas
  num_cols <- length(cols)
  
  # saco las filas que tienen (num_cols - 1) nulos
  filter_bool <- check_na_threshold(df_clean, num_cols-1)
  
  df_clean <- df_clean %>%  
    dplyr::filter(!filter_bool) %>% 
    mutate(
      provincia = trimws(ifelse(provincia_rubro %in% lista_provincias, provincia_rubro, NA)),
      rubro = ifelse(!(provincia_rubro %in% lista_provincias), provincia_rubro, "Total provincia")
    ) %>% 
    fill(provincia, .direction = "downup") %>% 
    select(-provincia_rubro) %>% 
    pivot_longer(!all_of(c('provincia', 'rubro')), 
                 names_to = 'anio', 
                 names_transform = as.integer,
                 values_to = "valor_expo",
                 values_transform = as.numeric)
  
  
  return(df_clean)
  
}


lista_provincias <- c("Buenos Aires",
                "Ciudad Autónoma de Buenos Aires",
                "Catamarca",
                "Chaco",
                "Chubut",
                "Córdoba",
                "Corrientes",
                "Entre Ríos",
                "Formosa",
                "Jujuy",
                "La Pampa",
                "La Rioja",
                "Mendoza",
                "Misiones",
                "Neuquén",
                "Río Negro",
                "Salta",
                "San Juan",
                "San Luis",
                "Santa Cruz",
                "Santa Fe",
                "Santiago del Estero",
                "Tierra del Fuego",
                "Tucumán",
                "Extranjero",
                "Indeterminado")



skip <- 8

all_sheets <- readxl::excel_sheets(argendataR::get_raw_path(fuente_raw))

df_clean <- purrr::map_dfr(.x = all_sheets, 
                           .f = function(x) clean_opex_provincia_rubro(sheet_name = x, skip = skip, lista_provincias = lista_provincias))

clean_filename <- glue::glue("{nombre_archivo_raw}_all_sheets_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 143
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) %>% 
  mutate(anio = as.integer(anio))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio","provincia")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
