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


id_fuente <- 238
fuente_raw <- sprintf("R%sC0",id_fuente)


sheet_name <- "Descriptores de actividad"


df_clean <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = sheet_name , col_names = F, skip = 3) %>%
  select(1:2) %>% 
  dplyr::filter(!is.na(as.numeric(`...1`)) | grepl("CIIU.* dígitos", `...1`)) %>% 
  mutate(
         codigo = ifelse(!is.na(as.numeric(`...1`)), as.numeric(`...1`), NA),
         descripcion = ifelse(!is.na(as.numeric(`...1`)), `...2`, NA),
         digitos = ifelse(grepl("CIIU.* dígitos", `...1`), as.numeric(str_extract(`...1`, "\\d{1}")), NA)
         ) %>% 
  fill(digitos, .direction = "down") %>% 
  drop_na(codigo) %>% 
  select(codigo, descripcion, digitos)


str_titulo <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = sheet_name , range = "A1:A1", col_names = F) %>% pull()


norm_title <- str_titulo %>% janitor::make_clean_names()

clean_filename <- glue::glue("{norm_title}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("OEDE - {str_titulo}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 146
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("codigo","digitos")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
