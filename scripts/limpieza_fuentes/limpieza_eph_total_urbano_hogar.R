#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 366
fuente_raw <- sprintf("R%sC0",id_fuente)


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

df_clean <- argendataR::get_raw_path(fuente_raw) %>%
  readr::read_csv() %>% 
  janitor::clean_names()


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)


id_fuente_clean <- 241
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean %>% 
                                       arrange(codusu, trim, ano4, provincia, aglomerado, nro_hogar) %>%
                                       group_by(trimestre, ano4) %>% 
                                       slice_head(n = 200) %>% 
                                       ungroup() %>% 
                                       select(codusu, trimestre, ano4, provincia, aglomerado, nro_hogar, pondera),
                                     df_clean_anterior %>% 
                                       arrange(codusu, trimestre, ano4, provincia, aglomerado, nro_hogar) %>%
                                       group_by(trimestre, ano4) %>% 
                                       slice_head(n = 200) %>% 
                                       ungroup() %>% 
                                       select(codusu, trimestre, ano4, provincia, aglomerado, nro_hogar, componente, pondera),
                                     pk = c('codusu', 'trimestre', 'ano4', 'provincia', 'aglomerado', 'nro_hogar')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion,
                        descripcion = "En la comparación se seleccionaron las key y la variable pondera, comparando muestras de 1000 registros")


