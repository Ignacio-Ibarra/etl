# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 95
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()



df_raw <- argendataR::get_raw_path(fuente_raw) %>% 
  read.csv(., sep = ";", fileEncoding = "iso-8859-1")


iso_codes <- argendataR::get_raw_path("R158C0") %>% 
  read_csv(.) %>% 
  select(iso2 = Alpha2code, iso3 = Alpha3code)


df_clean <- df_raw %>% 
  janitor::clean_names() %>% 
  mutate(unidad = "Millónes de dólares corrientes",
         valor = stringr::str_remove_all(millones_de_dolares, "\\.") %>% str_replace(., ",", ".") %>% as.double(.) ) %>% 
  left_join(iso_codes, join_by(codigo_pais == iso2)) %>% 
  select(anio = ano, cabps2010, descripcion_cabps2010, operacion, iso2 = codigo_pais, iso3, descripcion_pais, valor, unidad, observacion)


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name,
#                      descripcion = "Se pasa a minúscula nombres de columnas, la columna 'millones de dolares' se cambia a de nombre a 'valor' se agrega columna 'unidad' con el valor 'Millones de dólares corrientes', se corrigen los valores de dicha columna, se agrega columna 'iso3'"
#                      )

id_fuente_clean <- 187
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c('anio','iso2','cabps2010', 'operacion')
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
