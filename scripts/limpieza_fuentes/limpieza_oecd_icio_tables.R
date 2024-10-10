#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 226
fuente_raw <- sprintf("R%sC0",id_fuente)

# nomenclador <- argendataR::get_nomenclador_geografico() %>%  
#   select(iso3 = codigo_fundar, m49_code = m49_code_unsd, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) %>% 
#   dplyr::filter(!is.na(m49_code))


df_clean <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), sheet = "Area_Activities", range = "H3:K48") %>% 
  janitor::clean_names()

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]


clean_filename <- glue::glue("{nombre_archivo_raw}_area_activites_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Codigos Industrias")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 96, path_clean = clean_filename, directorio = tempdir(), nombre = clean_title, script = code_name)