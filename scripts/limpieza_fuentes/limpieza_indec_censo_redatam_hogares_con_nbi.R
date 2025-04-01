#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 249
fuente_raw <- sprintf("R%sC0",id_fuente)


df_raw <- readxl::read_excel(get_raw_path(fuente_raw), 
                             range = "A14:D3708",
                             col_names = F)

cleaning_func <- function(df_raw){
  
  col1 <- df_raw$...2
  
  start_ids <- grep("AREA #.*", col1) 
  
  codigos <- col1[start_ids] %>% str_replace(., "AREA # ", "")
  casos_si <- as.numeric(df_raw$...3[start_ids+3])
  casos_no <- as.numeric(df_raw$...3[start_ids+4])

  df <- data.frame(id_depto = codigos, cant_hogares_con_nbi = casos_si, cant_hogares_sin_nbi = casos_no)

  df$porcentaje_hogares_con_nbi <- 100*df$cant_hogares_con_nbi / (df$cant_hogares_con_nbi + df$cant_hogares_sin_nbi) 
  
  return(df %>% select(-cant_hogares_sin_nbi))
}


df_clean <- cleaning_func(df_raw = df_raw)

# Guardado de archivo
nombre_archivo_raw <- sub("\\.[^.]*$", "", fuentes_raw() %>% 
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
#                      path_clean = clean_filename,
#                      nombre = clean_title,
#                      script = code_name,
#                      descripcion = "El dataset contiene la cantidad de hogares y porcentaje sobre el total de hogares que poseen al menos un indicador NBI, por departamento, obtenido de REDATAM 2024-10-28")

actualizar_fuente_clean(id_fuente_clean = 118,
                        path_clean = clean_filename,
                        nombre = clean_title, 
                        script = code_name)