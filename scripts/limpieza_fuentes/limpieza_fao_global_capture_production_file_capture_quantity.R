# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 321
fuente_raw <- sprintf("R%sC0",id_fuente)


# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


filename <- "Capture_Quantity"

filename_ext <- glue::glue("{filename}.csv")

unzip(argendataR::get_raw_path(fuente_raw), files = filename_ext , exdir = tempdir())

path <- glue::glue("{tempdir()}/{filename_ext}")

df_clean <- path %>% 
  read.csv(., sep = ",") %>% 
  janitor::clean_names()



clean_filename <- glue::glue("{nombre_archivo_raw}_{tolower(filename)}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

clean_title <- glue::glue("{titulo.raw} - File: {filename}")

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      nombre = clean_title,
#                      path_clean = clean_filename,
#                      script = code_name,
#                      descripcion = "Se pasa a minúscula nombres de columnas"
#                      )

id_fuente_clean <- 193
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean )) 


comparacion <- comparar_fuente_clean(df_clean %>% 
                                       arrange(country_un_code, species_alpha_3_code, area_code, measure, period) %>% 
                                       slice_head(n = 1000),
                                     df_clean_anterior %>% 
                                       arrange(country_un_code, species_alpha_3_code, area_code, measure, period) %>% 
                                       slice_head(n = 1000),
                                     pk = c("country_un_code","species_alpha_3_code","area_code","measure","period")
)


actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion)
