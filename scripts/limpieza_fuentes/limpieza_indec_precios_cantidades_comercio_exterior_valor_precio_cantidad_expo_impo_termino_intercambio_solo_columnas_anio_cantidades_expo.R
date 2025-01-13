#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


id_fuente <- 44
fuente_raw <- sprintf("R%sC0",id_fuente)

# Guardado de archivo
nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()


df_clean <- readxl::read_excel(argendataR::get_raw_path(fuente_raw), skip = 3) %>% 
  janitor::clean_names() %>% 
  dplyr::select(anio = 1, cantidades_de_exportacion = 4) %>% 
  dplyr::mutate(anio = stringr::str_remove(anio, '\\*')) %>% 
  tidyr::drop_na() %>% 
  dplyr::mutate_all(as.numeric)


clean_filename <- glue::glue("{nombre_archivo_raw}_cantidades_expo.parquet")

clean_title <- glue::glue("{titulo.raw} - Indice de Cantidades de Exportacion")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)


id_fuente_clean <- 23
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


df_clean_anterior <- arrow::read_parquet(get_clean_path(codigo = codigo_fuente_clean ))


comparacion <- comparar_fuente_clean(df_clean,
                                     df_clean_anterior,
                                     pk = c("anio")
)

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name,
                        comparacion = comparacion, 
                        descripcion = "Se obtienen solo las columnas anio y cantidad de las exportaciones")




  