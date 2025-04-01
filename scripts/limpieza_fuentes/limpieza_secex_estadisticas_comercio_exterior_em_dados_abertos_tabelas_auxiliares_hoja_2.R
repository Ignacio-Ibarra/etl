# Este script obtiene los datos de la Hoja 2 del worksheet de la fuente raw R340C0


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 340
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

df_clean <- argendataR::get_raw_path(fuente_raw) %>% 
  readxl::read_excel(., sheet = "1") %>% 
  janitor::clean_names()

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - Códigos del Harmonized System y descripciones (en portugués, español e inglés)")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# 
# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "Los códigos a 8 dígitos sólo tienen descripción en portugués pero los códigos a 6 dígitos tienen descripción en español e inglés además")



id_fuente_clean <- 213
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('co_ncm')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)