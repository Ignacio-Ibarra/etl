# Codigo de limpieza de datos de cuadro de Jacques Charmes

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 300
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



df_raw <- readr::read_csv(argendataR::get_raw_path(fuente_raw)) %>% 
  janitor::clean_names() 


id_cols <- df_raw %>% colnames() %>% purrr::keep(., ~all(!grepl("y\\d+.*",.x)))

valores_col <- df_raw %>% colnames() %>% purrr::keep(., ~all(grepl("y\\d+$",.x)))

# flags_col <- df_raw %>% colnames() %>% purrr::keep(., ~all(grepl("y\\d+f$",.x))) # queda comentada esta parte porque no se la banca la memoria


anios_transform <- function(x){as.integer(str_extract(x, "^y(\\d+).*", group = 1))}

df_clean_valores <- df_raw %>% 
  select(all_of(c(id_cols, valores_col))) %>% 
  pivot_longer(!all_of(id_cols),
               names_to = 'year',
               names_transform = anios_transform,
               values_to = "value") 

# df_clean_flags <- df_raw %>% 
#   select(all_of(c(id_cols, flags_col))) %>% 
#   pivot_longer(!all_of(id_cols),
#                names_to = 'year',
#                names_transform = anios_transform,
#                values_to = "flags")  

pks <- c(id_cols, 'year')

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  drop_na(m49_code_unsd) %>% select(iso3 = codigo_fundar, area_code_m49 = m49_code_unsd, pais = desc_fundar)

df_clean_landing <- df_clean_valores %>% 
  # left_join(df_clean_flags, by = pks) %>% 
  mutate(
    area_code_m49 = as.integer(str_remove(area_code_m49, "^\\'")),
    item_code_fbs = str_remove(item_code_fbs, "^\\'")
  ) 


rm(
  # df_clean_flags, 
  # df_clean_notas, 
  df_clean_valores)

codigos <- df_clean_landing %>% distinct(area_code_m49) %>% 
  left_join(geonomenclador, join_by(area_code_m49)) %>% 
  drop_na(iso3)


df_clean <- df_clean_landing %>% 
  right_join(codigos, join_by(area_code_m49)) %>% 
  select(iso3, pais, year, item_code, item, element_code, element, unit, value
         # , flags, notes
         )

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_title <- glue::glue("{titulo.raw} - Países")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name)



id_fuente_clean <- 168
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('iso3', 'year', 'item_code', 
                                            'element_code', 
                                            'unit')
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)