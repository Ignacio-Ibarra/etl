#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 39
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

white_cols <- function(df) {
  sapply(df, function (col) all(is.na(col)))
}

pob_indec <- readxl::read_excel(argendataR::get_raw_path(fuente_raw))


pob_indec <- pob_indec[-c(1:4),]


names(pob_indec) <- pob_indec[1,] %>%
  janitor::make_clean_names()

pob_indec <- pob_indec %>%
  dplyr::rename(anio = na)

pob_indec <- pob_indec %>%
  dplyr::mutate(anio = as.numeric(gsub(" .*", "", anio )))


pob_indec <- pob_indec %>%
  dplyr::filter(!is.na(anio))

pob_indec <- pob_indec[,!sapply(pob_indec,
                                function(x) {sum(is.na(x)) == length(x)})]


pob_indec <- pob_indec %>% 
  pivot_longer(-c(anio),
               values_to = "valor",
               names_to = "indicador") %>% 
  mutate(valor = as.numeric(valor))

df_clean <- pob_indec %>% 
  filter(!is.na(anio))

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# 
# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 8
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'indicador')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)

