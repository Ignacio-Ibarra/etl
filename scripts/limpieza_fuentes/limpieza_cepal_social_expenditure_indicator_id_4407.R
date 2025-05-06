#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 404
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo

pattern <- fuentes_raw() %>% 
  pull(path_raw) %>% 
  tools::file_ext(.) %>%
  unique() %>% 
  keep(., ~all(.x != '')) %>% 
  paste0(., collapse = "|") %>% 
  paste0("(.*)\\.(",.,")$")


nombre_archivo_raw <- str_extract(fuentes_raw() %>% 
                                    dplyr::filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), 
                                  pattern = pattern, 
                                  group = 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

raw_data <- argendataR::get_raw_path(fuente_raw) %>% 
  jsonlite::read_json(.)

dimensions <- raw_data$body$dimensions 

dim_79338_df <- dimensions[[2]]$members %>% bind_rows() %>% select(id, fuente = name)

dim_79441_df <- dimensions[[3]]$members %>% bind_rows() %>% select(id, programa = name, programa_padre = parent)

dim_29117_df <- dimensions[[4]]$members %>% bind_rows() %>% select(id, anio = name)

notes_df <- raw_data$body$footnotes %>% bind_rows() %>% rename(nota = description)

df_raw <- raw_data$body$data %>% bind_rows()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

df_clean <- df_raw %>% 
  left_join(dim_79338_df, join_by(dim_79338 == id)) %>% 
  left_join(dim_79441_df, join_by(dim_79411 == id)) %>% 
  left_join(dim_29117_df, join_by(dim_29117 == id)) %>%
  left_join(geo_front, join_by(iso3)) %>% 
  mutate(valor = as.numeric(value),
         notes_ids = as.integer(notes_ids)) %>%
  left_join(notes_df, join_by(notes_ids == id)) %>% 
  select(anio, iso3, pais_nombre, fuente_id=dim_79338, fuente, programa_id=dim_79411, programa, programa_padre, valor, nota)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw}")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 255
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio','iso3','fuente_id','programa_id'))


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)