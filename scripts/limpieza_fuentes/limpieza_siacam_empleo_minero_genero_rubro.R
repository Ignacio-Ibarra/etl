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

code_name <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 275
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


tmp_file <- tempfile(pattern = "mineria", tmpdir = tempdir(), fileext = ".csv")

lineas <- readLines(argendataR::get_raw_path(fuente_raw), encoding='latin1')
lineas <- gsub('\"', '', lineas)
writeLines(lineas, tmp_file)

df_clean <- read.csv(tmp_file, sep=";", na.strings = c("NA", "#N/A"), dec = ",") %>% 
  janitor::clean_names()

fechas_corregidas <- data.frame(
  fechas_num = df_clean %>% arrange(ano_mes) %>% distinct(ano_mes) %>% pull()
)

fechas_corregidas$anio_mes <- seq.Date(from = as.Date("2007-01-01"), by = "month", length.out = nrow(fechas_corregidas))

df_clean <- df_clean %>% 
  left_join(fechas_corregidas, join_by(ano_mes == fechas_num)) %>% 
  mutate(
    provincia = case_when(
      provincia_zona == "Cordoba" ~ "Córdoba",
      provincia_zona == "Entre Rios" ~ "Entre Ríos",
      provincia_zona == "Neuquen" ~ "Neuquén",
      provincia_zona == "Rio Negro" ~ "Río Negro",
      provincia_zona == "Santiago Del Estero" ~ "Santiago del Estero",
      provincia_zona == "Tierra Del Fuego" ~ "Tierra del Fuego",
      provincia_zona == "Tucuman" ~ "Tucumán",
      provincia_zona == "Santa Fe" ~ "Santa Fe",
      TRUE ~ provincia_zona
    )) %>% 
  select(-ano_mes,-provincia_zona)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 144
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio_mes", "provincia", "genero", "rubro")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
