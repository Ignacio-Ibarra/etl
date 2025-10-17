#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 225
fuente_raw <- sprintf("R%sC0",id_fuente)

# nomenclador <- argendataR::get_nomenclador_geografico() %>%  
#   select(iso3 = codigo_fundar, m49_code = m49_code_unsd, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) %>% 
#   dplyr::filter(!is.na(m49_code))


df_clean <- read_csv(argendataR::get_raw_path(fuente_raw)) %>% 
  janitor::clean_names() %>% 
  select(iso3 = ref_area, anio = time_period, sector = activity, vab_usd = obs_value)
  
  

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      script = code_name,
#                      path_clean = clean_filename)

id_fuente_clean <- 97
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'iso3', 'sector')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
