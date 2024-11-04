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


id_fuente <- 38
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


# cuadro 12 ---------------------------------------------------------------




sheet_name <- 'cuadro 12'




pib_categoria_pcorr <- readxl::read_excel(get_raw_path(fuente_raw),
                                          sheet = sheet_name)

pib_categoria_pcorr <- pib_categoria_pcorr[-c(1:2),] %>%
  t() %>%
  tibble::as_tibble(.name_repair = "unique")

names(pib_categoria_pcorr) <- pib_categoria_pcorr[1,] %>%
  janitor::make_clean_names()

pib_categoria_pcorr <- pib_categoria_pcorr %>%
  dplyr::rename(anio = na, trim = na_2)

pib_categoria_pcorr <- pib_categoria_pcorr %>%
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))

pib_categoria_pcorr <- pib_categoria_pcorr %>%
  tidyr::fill(anio)

pib_categoria_pcorr <- pib_categoria_pcorr %>%
  dplyr::filter(!is.na(trim))

pib_categoria_pcorr <- pib_categoria_pcorr[,!sapply(pib_categoria_pcorr,
                                                    function(x) {sum(is.na(x)) == length(x)})]


colnames(pib_categoria_pcorr) <- gsub("_\\d$","",colnames(pib_categoria_pcorr))

df_clean <- pib_categoria_pcorr %>% 
  pivot_longer(-c(anio, trim),
               values_to = "valor",
               names_to = "indicador") %>% 
  mutate(valor = 1E6*as.numeric(valor),
         unidad = "pesos corrientes") %>% 
  relocate(unidad, .after = trim)


normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - {sheet_name}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# carga fuente limpia en el drive

# agregar_fuente_clean(id_fuente_raw = 38,
#                      path_clean = "oferta_demanda_pctes.csv",
#                      nombre = "Oferta y Demanda Globales trimestrales a precios 2004",
#                      script = "limpieza_pib_oyd_pctes_indec.R")



id_fuente_clean <- 7
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "trim", "indicador", "unidad")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)




