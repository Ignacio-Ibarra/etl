
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


id_fuente <- 294
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



limpiar_sheet <- function(sheet_name, last_year ){
  
  anios_base <- 2010:last_year
  
  cols <- c("pais_nombre", anios_base)
  
  regions <- c(
    'Central & South America' = 'América del Sur y Central', 
    'Europe' = 'Europa', 
    'Africa' = 'África', 
    'Asia' = 'Asia', 
    'Oceania' = 'Oceanía', 
    'North America' = 'América del Norte', 
    'Commonwealth of Independent States' = 'Comunidad de Estados Independientes')
  
  sheet_data <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = sheet_name, skip = 4, col_names = F)
  
  names(sheet_data) <- cols
  
  df <- sheet_data %>% 
    dplyr::filter(!(pais_nombre %in% c("Sub-total", "Global Total"))) %>% 
    dplyr::filter(!is.na(pais_nombre)) %>% 
    mutate(
      region = ifelse(pais_nombre %in% names(regions), regions[pais_nombre], NA)
    ) %>% 
    fill(region, .direction = "down") %>%
    dplyr::filter(!(pais_nombre %in% names(regions))) %>% 
    pivot_longer(
      matches("\\d{4}"),
      names_to = 'anio',
      names_transform = as.integer,
      values_to = 'toneladas',
      values_transform = as.numeric
    ) %>% 
    mutate(
      pais_nombre = case_when(
        pais_nombre == "Other" ~ paste0("Otros países de ", region),
        TRUE ~ pais_nombre
      )
    )
  
  
  return(df)
  
}

last_year <- year(Sys.Date()) - 1
df_raw <- limpiar_sheet(sheet_name = 'Data', last_year = last_year)

# Traduccion de nombres de países. 
geonomenclador <- argendataR::get_nomenclador_geografico() %>% select(iso3 = codigo_fundar, pais_nombre = desc_fundar, es_iso) %>% 
  filter(es_iso == 1) %>% 
  select(-es_iso)

nombres_wgo <- df_raw %>% distinct(pais_nombre) %>% 
  dplyr::filter(!grepl("Otros países.*", pais_nombre)) %>% 
  left_join(geonomenclador, join_by(pais_nombre)) %>% 
  mutate(esta = !is.na(iso3))



# Instala las librerías necesarias si no las tienes
if (!requireNamespace("tm", quietly = TRUE)) install.packages("tm")
if (!requireNamespace("proxy", quietly = TRUE)) install.packages("proxy")
if (!requireNamespace("textTinyR", quietly = TRUE)) install.packages("textTinyR")
if (!requireNamespace("slam", quietly = TRUE)) install.packages("slam")

library(tm)
library(proxy) # Para calcular similitudes coseno
library(slam) # Para operaciones con matrices dispersas


# Función para normalizar cadenas
normalize_text <- function(text) {
  text <- tolower(text)                  # Convertir a minúsculas
  text <- stringi::stri_trans_general(text, "Latin-ASCII") # Eliminar acentos
  text <- gsub("[[:punct:]]", "", text)  # Eliminar puntuaciones
  text <- gsub("[[:digit:]]", "", text)  # Eliminar números
  text <- gsub("\\s+", " ", text)        # Quitar espacios extras
  trimws(text)                           # Eliminar espacios al principio y al final
}
# Función para generar n-gramas
generate_ngrams <- function(text, n = 2) {
  chars <- unlist(strsplit(text, split = ""))
  if (length(chars) < n) {
    return(chars) # Si la longitud es menor que n, retorna los caracteres originales
  }
  sapply(1:(length(chars) - n + 1), function(i) paste(chars[i:(i + n - 1)], collapse = ""))
}

# Función para construir matriz TF-IDF basada en documentos y vocabulario
build_tfidf_matrix <- function(documents, vocabulary) {
  # Crear una lista de n-gramas por documento
  tokenized_docs <- lapply(documents, function(doc) generate_ngrams(doc, n = 2))
  
  # Construir la matriz dispersa
  doc_term_matrix <- sapply(tokenized_docs, function(ngrams) {
    table(factor(ngrams, levels = vocabulary)) # Contar ocurrencias en base al vocabulario
  })
  doc_term_matrix <- t(doc_term_matrix)
  # Convertir la matriz a formato `DocumentTermMatrix`
  # dtm <- DocumentTermMatrix(SimpleCorpus(VectorSource(documents)), control = list(dictionary = vocabulary))
  dtm <- as.DocumentTermMatrix(doc_term_matrix, weighting = weightTfIdf)
  
  return(as.matrix(dtm))
}

# Voy a buscar nombres posibles en los nombres que tenemos en geonomenclador
# Del universo total de nombres quito los nombres que matchearon con la base
# de Prados de la Escosura

isos_encontrados <- nombres_wgo %>% filter(esta) %>% pull(iso3)
paises_busqueda <- geonomenclador %>% filter(!(iso3 %in% isos_encontrados))
A <- paises_busqueda %>% pull(pais_nombre)
B <- nombres_wgo %>% filter(!esta) %>% pull(pais_nombre)  # Nombres sucios


# Guardar las versiones originales para los resultados finales
A_original <- A
B_original <- B

# Normalizar las cadenas
A <- sapply(A, normalize_text)
B <- sapply(B, normalize_text)


# Tokenizar documentos limpios para construir el vocabulario
tokenized_clean <- unlist(lapply(A, function(doc) generate_ngrams(doc, n = 2)))
vocabulary <- unique(tokenized_clean)

# Construir matrices TF-IDF
tfidf_clean <- build_tfidf_matrix(A, vocabulary)
tfidf_dirty <- build_tfidf_matrix(B, vocabulary)


# Calcular similitudes coseno
cosine_sim <- proxy::simil(tfidf_clean, tfidf_dirty,method = "cosine")

threshold <- 0.5

# Convertir la matriz de similitudes a un dataframe para obtener las mejores coincidencias
match_results <- as.data.frame(as.table(as.matrix(cosine_sim))) %>%
  rename(clean_index = Var1, dirty_index = Var2, similarity = Freq) %>%
  mutate(clean_name = A[as.integer(clean_index)], dirty_name = B[as.integer(dirty_index)]) %>%
  group_by(dirty_index) %>%
  dplyr::filter(similarity>threshold & dirty_index != 'Iceland') %>%  #Similarity(Iceland, Belice) > Similarity(Belize, Belice)
  slice_max(similarity, n = 1) %>% 
  ungroup() %>%
  group_by(clean_index) %>% 
  slice_max(similarity, n = 1) %>% 
  ungroup() %>% 
  arrange(dirty_index) %>%
  select(clean_index, dirty_index, similarity)

match_results[match_results$dirty_index == "DR Congo", c("clean_index")] <- "República Democrática del Congo"
match_results[match_results$dirty_index == "Dominican Republic", c("clean_index")] <- "Dominicana"


correccion <- match_results %>% 
  left_join(paises_busqueda, join_by(clean_index == pais_nombre))


correccion_vec <- setNames(correccion$iso3, correccion$dirty_index)


completar_automaticamente <- nombres_wgo %>% 
  select(-esta) %>% 
  mutate(
    iso3 = ifelse(is.na(iso3), correccion_vec[pais_nombre], iso3)
  ) %>% 
  dplyr::filter(!is.na(iso3))



completar_automaticamente <- setNames(completar_automaticamente$iso3, completar_automaticamente$pais_nombre)

completar_a_mano <- c(
  'United States' = 'USA',
  'Czechoslovakia/Czechia' = 'CZE',
  'Denmark' = 'DNK',
  'Latvia' = 'LVA',
  'Norway' = 'NOR',
  'Sweden' = 'SWE',
  'Ukraine' = 'UKR',
  'UK' = 'GBR',
  'Algeria' = 'DZA',
  "Côte d'Ivoire" = 'CIV',
  'Morocco' = 'MAR',
  'Tunisia' = 'TUN',
  'Korea, South' = 'KOR',
  'Syrian Arab R' = 'SYR',
  'USA' = 'USA',
  'Germany' = 'DEU',
  'Ireland' = "IRL",
  'Netherlands' = 'NLD',
  "Slovak Rep"  = 'SVK',
  'CAR' = 'CAF',
  'Congo Dem R' = 'COD',
  "Switzerland" = 'CHE',
  'Equat. Guinea' = 'GNQ',
  "Rwanda" = 'RWA',
  'South Africa' = 'ZAF',
  'Lebanon' = 'LBN',
  'Turkey' = 'TUR',
  'U A Emirates' = 'ARE',
  'Cyprus' = 'CYP',
  'Iceland' = 'ISL',
  'Romania' = 'ROU'
)

completar <- c(completar_automaticamente, completar_a_mano)

geonomenclador_vec <- setNames(geonomenclador$pais_nombre, geonomenclador$iso3)

df_clean <- df_raw %>% 
  mutate(
    iso3 = completar[pais_nombre],
  ) %>% 
  mutate(
    pais_nombre = case_when(
      is.na(iso3) ~ pais_nombre,
      TRUE ~ geonomenclador_vec[iso3]
    )
  ) %>% 
  select(anio, iso3, pais_nombre, region, toneladas)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 163
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio", "pais_nombre"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
