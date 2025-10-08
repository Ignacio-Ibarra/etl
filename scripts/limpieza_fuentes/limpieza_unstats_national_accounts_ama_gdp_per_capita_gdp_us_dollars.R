# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 454
fuente_raw <- sprintf("R%sC0",id_fuente)

df_stage<- argendataR::get_raw_path(fuente_raw) %>% 
  readr::read_csv(.)

geonomneclador <- argendataR::get_nomenclador_geografico() %>% 
  dplyr::filter(!is.na(m49_code_unsd)) %>% 
  select(iso3 = codigo_fundar, m49_code_unsd, pais_nombre = desc_fundar)


df_clean <- df_stage %>% 
  select(country_id = countryCode, countryName, anio = fiscalYear, 
         indicator_name = serieName, value = observationValue, 
         note = observationNote,
         titulo = itemName) %>% 
  left_join(geonomneclador, join_by(country_id == m49_code_unsd)) %>% 
  select(iso3, pais_nombre, country_en = countryName, anio, indicator_name, value, note, titulo)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)

id_fuente_clean <- 297
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio', 'iso3', 'indicator_name')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)



