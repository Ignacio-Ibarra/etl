#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 217
fuente_raw <- sprintf("R%sC0",id_fuente)



df_raw <- read_csv(get_raw_path(fuente_raw)) 



df_clean <- df_raw %>% 
  rename(iso3 = country_code, anio = reporting_year) 

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

clean_filename <- glue::glue("{nombre_archivo_raw}_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_clean(id_fuente_raw = id_fuente,
#                      df = df_clean,
#                      path_clean = clean_filename,
#                      nombre = "Ingreso per cápita diario por año, país, tipo de bienestar y nivel de reporte",
#                      script = code_name)

comparacion <- comparar_fuente_clean(df_clean, id = 89, pk = c("iso3", "anio", "reporting_level", "welfare_type"))

actualizar_fuente_clean(id_fuente_clean = 89,
                        comparacion = comparacion,
                        path_clean = clean_filename, 
                        directorio = tempdir(),
                        descripcion = "Para el codigo XKX se pone el código KOS")
