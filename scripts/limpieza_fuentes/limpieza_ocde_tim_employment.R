#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


id_fuente <- 232
fuente_raw <- sprintf("R%sC0",id_fuente)

# nomenclador <- argendataR::get_nomenclador_geografico() %>%  
#   select(iso3 = codigo_fundar, m49_code = m49_code_unsd, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) %>% 
#   dplyr::filter(!is.na(m49_code))


df_raw <- read_csv(argendataR::get_raw_path(fuente_raw)) 


df_clean <- df_raw %>% 
  select(`REF_AREA: Reference area`, `ACTIVITY: Economic activity`, anio = `TIME_PERIOD: Time period`, personas_empleadas_miles = OBS_VALUE) %>% 
  separate(`REF_AREA: Reference area`, into = c("iso3", "nombre_pais"), sep = ": ") %>% 
  separate(`ACTIVITY: Economic activity`, into = c("codigo_act", "desc_act"), sep = ": ") %>% 
  select(anio, iso3, codigo_act, desc_act, personas_empleadas_miles)


# Guardado de archivo
nombre_archivo_raw <-  sub("\\.[^.]*$", "", fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull())


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

actualizar_fuente_clean(id_fuente_clean = 103, 
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name)
