# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 302
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


df_raw <- read_csv(argendataR::get_raw_path(fuente_raw))

df_clean <- df_raw %>% 
  mutate(
    m49_code = as.integer(str_remove(countryId, "country-"))
  ) %>% 
  select(m49_code, atlas_id = legacyCountryId, iso3 = iso3Code, nombre_en = nameEn, nombre_corto_en = nameShortEn)

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - Países")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 170
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c( "m49_code","atlas_id","iso3")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)