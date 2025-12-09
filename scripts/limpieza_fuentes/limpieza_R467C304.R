# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

id_fuente <- 467
fuente_raw <- sprintf("R%sC0",id_fuente)
sheet_name <- "RESUMEN por MAQUINA"

df <-  readxl::read_xlsx(get_raw_path(fuente_raw), sheet = sheet_name, skip = 24)

df <- df %>% 
  janitor::clean_names()

df <- df %>% 
  select(1:14)

# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
                            filter(codigo == fuente_raw) %>% 
                            select(path_raw) %>% 
                            pull())

normalized_sheet_name <- sheet_name %>% janitor::make_clean_names(.)

clean_filename <- glue::glue("{nombre_archivo_raw}_{normalized_sheet_name}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df %>% arrow::write_parquet(., sink = path_clean)

titulo.raw <- fuentes_raw() %>% 
  filter(codigo == fuente_raw) %>% 
  select(nombre) %>% pull()

clean_title <- glue::glue("{titulo.raw} - Cuadro: {sheet_name}")

agregar_fuente_clean(id_fuente_raw = id_fuente,
                     df = df,
                     path_clean = clean_filename,
                     nombre = clean_title,
                     script = code_name)

id_fuente_clean <- 304
codigo_fuente_clean <- sprintf("R%sC%s", id_fuente, id_fuente_clean)


# df_clean_anterior <- arrow::read_parquet(argendataR::get_clean_path(codigo = codigo_fuente_clean )) 

# comparacion <- comparar_fuente_clean(df_clean,
#                                      df_clean_anterior,
#                                      pk = c('maquina', 'central', 'agente')
# )


c <- comparar_fuente_clean(df, df, pk =c("ano"))

actualizar_fuente_clean(id_fuente_clean = id_fuente_clean,
                        path_clean = clean_filename,
                        nombre = clean_title,
                        script = code_name,
                        comparacion = list("Fix script name", "Sin cambios"))
