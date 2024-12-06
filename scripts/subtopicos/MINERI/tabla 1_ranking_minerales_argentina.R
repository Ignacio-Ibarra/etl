################################################################################
##                              Dataset: nombre                               ##
################################################################################

# Este script es una copia del script ~/etl/scripts/subtopicos/ACECON/7_pib_comp_va.R


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "ranking_minerales_argentina"
analista = "Kevin Corfield"
fuente1 <- "R264C134" # USGS world.zip
fuente2 <- "R294C163" # gold.org
fuente3 <- "R295C0" # PDF SECMIN no se usa en el script


geonomenclador <- argendataR::get_nomenclador_geografico()


minerales_es = c(
  'boron' = 'Boro',
  'diato' = 'Diatomita',
  'lithi' = 'Litio', 
  'silve' = 'Plata',
  'molyb' = "Molibdeno",
  'perli' = 'Perlita',
  'sandi' = 'Arena y Grava (industrial)',
  'silve' = 'Plata',
  'stron' = 'Estroncio',
  'alumi' = 'Aluminio', 
  'prase' = 'Praseodimio',
  'zirco' = 'Circonio',
  'chrom' = 'Cromo', 
  'cobal' = 'Cobalto', 
  'coppe' = 'Cobre', 
  'feore' = 'Mineral de Hierro',
  'graph' = 'Grafito',
  'dyspr' = 'Disprosio',
  'titan' = 'Titanio',
  'yttri' = 'Itrio',
  'zinc' =  'Cinc',
  'arsen' = 'Arsénico',
  'terbi' = 'Terbio',
  'tellu' = 'Telurio',
  'silic' = 'Silicio',
  'lanth' = 'Lantano',
  'iridi' = 'Iridio',
  'indiu' = 'Indio',
  'hafni' = 'Hafnio',
  'galli' = 'Galio',
  'gold' =  'Oro', 
  'lead' =  'Plomo', 
  'manga' = 'Manganeso',
  'magne' = 'Magnesio',
  'nicke' = 'Níquel', 
  'palla' = 'Paladio', 
  'plati' = 'Platino', 
  'tin' =   'Estaño', 
  'vanad' = 'Vanadio',
  'cadmi' = 'Cadmio',
  'antim' = 'Antimonio',
  'mercu' = 'Mercurio', 
  'tungs' = 'Tungsteno',
  'raree' = "Tierras Raras",
  'neody' = 'Neodimio',
  'niobi' = 'Niobio',
  'selen' = 'Selenio',
  'rare ' = 'Tierras Raras',
  'other' = 'Otros',
  'asbes' = 'Asbestos',
  'barit' = 'Baritina',
  'bauxi' = 'Bauxita'
)


country_names <- c(
  "China" = "China",
  "India" = "India",
  "Canada" = "Canadá",
  "United Arab Emirates" = "Emiratos Árabes Unidos",
  "Bahrain" = "Bahrein",
  "Australia" = "Australia",
  "Norway" = "Noruega",
  "Malaysia" = "Malasia",
  "United States" = "Estados Unidos",
  "Brazil" = "Brasil",
  "Iceland" = "Islandia",
  "South Africa" = "Sudáfrica",
  "Turkey" = "Turquía",
  "Finland" = "Finlandia",
  "Congo" = "República Democrática del Congo",
  "Indonesia" = "Indonesia",
  "Philippines" = "Filipinas",
  "Cuba" = "Cuba",
  "Madagascar" = "Madagascar",
  "Papua New Guinea" = "Papua Nueva Guinea",
  "New Caledonia" = "Nueva Caledonia",
  "Chile" = "Chile",
  "Peru" = "Perú",
  "Zambia" = "Zambia",
  "Mexico" = "México",
  "Poland" = "Polonia",
  "Sweden" = "Suecia",
  "Ukraine" = "Ucrania",
  "Mauritania" = "Mauritania",
  "Uzbekistan" = "Uzbekistán",
  "Ghana" = "Ghana",
  "Mali" = "Malí",
  "Burkina Faso" = "Burkina Faso",
  "Tanzania" = "Tanzanía",
  "Bolivia" = "Bolivia",
  "Tajikistan" = "Tayikistán",
  "Gabon" = "Gabón",
  "Côte d’Ivoire" = "Costa de Marfil",
  "Burma" = "Myanmar",
  "Georgia" = "Georgia",
  "Vietnam" = "Vietnam",
  "Zimbabwe" = "Zimbabwe",
  "Nigeria" = "Nigeria",
  "Rwanda" = "Ruanda",
  "Laos" = "Lao",
  "Guatemala" = "Guatemala",
  "Kyrgyzstan" = "Kirguistán",
  "Pakistan" = "Pakistán",
  "Argentina" = "Argentina",
  "Portugal" = "Portugal",
  "Morocco" = "Marruecos",
  "Peru" = "Perú",
  "Austria" = "Austria",
  "Korea, North" = "Corea del Norte",
  "Spain" = "España",
  "Kazakhstan" = "Kazajstán",
  "Russia" = "Rusia",
  "Ukraine" = "Ucrania",
  "New Caledonia9" = "Nueva Caledonia",
  "Korea, Republic of" = "Corea del Sur",
  "Thailand" = "Tailandia",
  "Other countries" = "Otros países",
  "France" = "Francia",
  "Germany" = "Alemania",
  "Denmark" = "Dinamarca",
  "New Zeland" = "Nueva Zelandia",
  "Hungary" = "Hungría",
  "Czechia" = "Chequia",
  "Italy" = "Italia",
  "Japan" = "Japón",
  "Netherlands" = "Países Bajos",
  "Iran" = "Irán"
  )



df_usgs_world <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

df_gold <- arrow::read_parquet(argendataR::get_clean_path(fuente2))

df_usgs_produccion <- df_usgs_world %>% 
  dplyr::filter(!grepl(".*prod_note.*", variable)) %>% 
  dplyr::filter(grepl(".*prod_.*", variable)) %>% 
  mutate(anio = as.numeric(str_extract(variable, pattern = ".*(\\d{4}).*", group = 1))) %>% 
  mutate(value_num = as.numeric(str_replace_all(trimws(value), ",", ""))) %>% 
  mutate(unit = ifelse(id == "alumi", "thousand metric tons", unit)) %>% 
  drop_na(value_num) %>% 
  mutate(value = value_num) %>% 
  select(-c(value_num,variable)) %>% 
  mutate(id = ifelse(grepl(".*Palladium.*", type), "palla", id)) %>% 
  group_by(id, type, country) %>% 
  fill(unit, .direction = "downup") %>% 
  ungroup() 

argentina_vs_otros <- df_usgs_produccion %>% 
  dplyr::filter(anio == max(anio)) %>% 
  dplyr::filter(!grepl("World.*", country)) %>% 
  mutate(
    mineral = recode(id, !!!minerales_es)
  ) %>% 
  group_by(id, anio) %>% 
  filter( any(str_detect(country, "Argentina")) | type == "Mine production, recoverable copper content") %>%  # filtro los mercados donde argentina existe y el de Cobre
  ungroup() %>%
  mutate(country = trimws(country)) %>% 
  mutate(country_es = recode(country, !!!country_names)) %>% 
  left_join(geonomenclador  %>%
              dplyr::filter(fuente_codigo_fundar == 'iso') %>%
              select(codigo_pais = codigo_fundar, country_es = desc_fundar),
            join_by(country_es)) %>% 
  group_by(country) %>% 
  fill(codigo_pais, .direction = "downup") %>% 
  fill(country_es, .direction = "downup") %>% 
  ungroup() 



# Corrijo nombres de countries que están mal escritos 

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
  text <- tolower(text)               # Convertir a minúsculas
  text <- gsub("[[:punct:]]", "", text) # Eliminar puntuaciones
  text <- gsub("[[:digit:]]", "", text) # Eliminar números
  text <- gsub("\\s+", " ", text)     # Quitar espacios extras
  trimws(text)                        # Eliminar espacios al principio y al final
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


# Vectores de ejemplo
A <- names(country_names)
B <- argentina_vs_otros %>% dplyr::filter(is.na(codigo_pais)) %>% distinct(country) %>% pull()  # Nombres sucios


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


threshold <- 0.6

# Convertir la matriz de similitudes a un dataframe para obtener las mejores coincidencias
match_results <- as.data.frame(as.table(as.matrix(cosine_sim))) %>%
  rename(clean_index = Var1, dirty_index = Var2, similarity = Freq) %>%
  mutate(clean_name = A[as.integer(clean_index)], dirty_name = B[as.integer(dirty_index)]) %>%
  group_by(dirty_index) %>%
  slice_max(similarity, n = 1) %>% 
  ungroup() %>%
  arrange(dirty_index) %>%
  select(clean_index, dirty_index, similarity)


countries_correccion <- stats::setNames( as.character(match_results$clean_index) , as.character(match_results$dirty_index) )
countries_ok <- setNames(names(country_names), names(country_names))

country_names_recoder <- c(countries_correccion, countries_ok)

df_final <- argentina_vs_otros %>% 
  mutate(country = recode(country, !!!country_names_recoder)) %>% 
  mutate(country_es = recode(country, !!!country_names)) %>% 
  select(-codigo_pais) %>% 
  left_join(geonomenclador  %>%
              dplyr::filter(fuente_codigo_fundar == 'iso') %>%
              select(codigo_pais = codigo_fundar, country_es = desc_fundar),
            join_by(country_es))



df_oro <- df_gold %>% 
  rename(codigo_pais = iso3,
         pais = pais_nombre,
         produccion = toneladas) %>% 
  mutate(
   unidad_medida = 'metric tons',
    mineral = 'Oro'
  ) %>% 
  dplyr::filter(anio == max(df_final$anio)) %>% 
  dplyr::filter(!is.na(codigo_pais)) %>% 
  select(-region,-anio)

# El cobre es un valor promedio de los la década del 2000
df_cobre_arg <- data.frame(
  codigo_pais = "ARG",
  pais = "Argentina",
  mineral = "Cobre",
  produccion = 176,
  unidad_medida = "thousand metric tons"
)

df_output <- df_final %>% 
  select(codigo_pais, pais = country_es, mineral, produccion = value, unidad_medida = unit) %>% 
  bind_rows(df_oro) %>% 
  bind_rows(df_cobre_arg) %>% 
  group_by(mineral) %>% 
  mutate(ranking = rank(-produccion, ties.method = "first")) %>% 
  ungroup() %>% 
  dplyr::filter(produccion > 0)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega")


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_output, #hago esto solo para poder pasar un dataset nuevo totalmente distinto. 
  df = df_output,
  nombre = output_name,
  pk = c("codigo_pais","mineral"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)



#-- Exportar Output ----

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("codigo_pais",
                      "pais",
                      "mineral",
                      "produccion",
                      "unidad_medida",
                      "ranking"),
  descripcion = c("Código de país según ISO 3166-1 alpha-3",
                  "Nombre de país",
                  "Nombre del mineral commodity",
                  "Volumen de la produccion",
                  "Unidad de medida",
                  "Ranking de la producción")
)

descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


aclaracion <- paste0("Se actualizó la fuente de información de 2022 a 2023.")

df_output_final %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("codigo_pais","mineral"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_geo_referencia = 'codigo_pais',
    descripcion_columnas = descripcion,
    aclaracion = aclaracion
  )

