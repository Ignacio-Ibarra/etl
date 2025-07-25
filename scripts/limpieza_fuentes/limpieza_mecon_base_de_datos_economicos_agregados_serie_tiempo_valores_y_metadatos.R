# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 306
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

filename <- "series-tiempo.csv"

unzip(argendataR::get_raw_path(fuente_raw), file = filename, exdir = tempdir(), junkpaths = T) 

df_clean <- read_csv(file.path(tempdir(),filename))

clean_filename <- glue::glue("{nombre_archivo_raw}.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# 
# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)


id_fuente_clean <- 174
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))

A <- df_clean %>% 
  select(serie_id, indice_tiempo, indice_tiempo_frecuencia, valor) %>% 
  arrange(serie_id, indice_tiempo, indice_tiempo_frecuencia) %>% 
  slice_head(n = 1000)

B <- df_clean_anterior %>% 
  select(serie_id, indice_tiempo, indice_tiempo_frecuencia, valor) %>% 
  arrange(serie_id, indice_tiempo, indice_tiempo_frecuencia) %>% 
  slice_head(n = 1000)

comparacion <- comparar_fuente_clean(A, B, pk = c("serie_id", "indice_tiempo", "indice_tiempo_frecuencia"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
