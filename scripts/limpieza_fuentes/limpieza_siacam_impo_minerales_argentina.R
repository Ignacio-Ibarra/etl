
#limpio la memoria
rm( list=ls())  #Borro todos los objetos
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

code_name <- get_file_location() %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 270
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

df_clean <- read.csv(tmp_file, sep=";",
                     na.strings = c("NA", "#N/A")) %>% 
  dplyr::filter(!is.na(ANYO)) %>% 
  janitor::clean_names() %>% 
  mutate(cif = as.numeric(gsub(",", ".", gsub("\\.", "", cif))),
         anyo = as.integer(anyo)) %>% 
  drop_na(cif) %>% 
  mutate(
    origen = ifelse(trimws(origen) == "" | is.na(origen), "sin dato", trimws(origen)),
    procedenc = ifelse(trimws(procedenc) == "" | is.na(procedenc), "sin dato", trimws(procedenc)),
    ppro = ifelse(is.na(ppro), "sin dato", as.character(ppro))
  ) %>% 
  group_by(across(-cif)) %>% 
  summarise(cif = sum(cif, na.rm = T))

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 139
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anyo", "ppro", "procedenc", "mes", "grupo")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
