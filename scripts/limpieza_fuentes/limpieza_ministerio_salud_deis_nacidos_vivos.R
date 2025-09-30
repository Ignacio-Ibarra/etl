#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 437
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

rawlist <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(.)

df_stage <- rawlist %>% 
  bind_rows() 

diccionario_prov <- argendataR::get_raw_path("R84C0") %>% 
  read.csv() %>% 
  distinct(prov_cod, prov_desc)


df_clean <- df_stage %>% 
  janitor::clean_names() %>% 
  left_join(diccionario_prov, join_by(provres == prov_cod)) %>% 
  mutate(prov_desc = case_when(
    provres == 98 ~ "Otro país",
    provres == 99 ~ "Lugar no especificado",
    TRUE ~ prov_desc
  )) %>%
  mutate(across(where(is.character), ~ str_replace_all(.x, "m<e1>s", "más")))



clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 282
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = colnames(df_clean)[colnames(df_clean)!="cuenta"]
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)