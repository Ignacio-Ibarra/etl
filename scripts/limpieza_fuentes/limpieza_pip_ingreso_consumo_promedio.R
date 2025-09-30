#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 217
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


df_raw <- read_csv(argendataR::get_raw_path(fuente_raw)) 



df_clean <- df_raw %>% 
  rename(iso3 = country_code, anio = reporting_year) 


clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - Dataset limpio")


# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = "Ingreso per cápita diario por año, país, tipo de bienestar y nivel de reporte",
#                      script = code_name)


id_fuente_clean <- 89
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean  %>% 
                                       select(iso3, anio, reporting_level, welfare_type, mean),
                                     df_clean_anterior %>% 
                                       select(iso3, anio, reporting_level, welfare_type, mean),
                                     pk = c("iso3", "anio", "reporting_level", "welfare_type")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)


