#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

require(purrr)


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}



id_fuente <- 216
fuente_raw <- sprintf("R%sC0",id_fuente)



df_raw <- read_csv(get_raw_path(fuente_raw)) 

indicator_name <- df_raw %>% distinct(indicator) %>% pull() %>% sub(".*- ","",.)

df_clean <- df_raw %>% 
  mutate(iso3 = purrr::map_chr(country, .f = function(x){sub(" -.*","",x)}) ) %>% 
  select(iso3, anio=year, mean_years_schoolling = value)

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
#                      nombre = indicator_name,
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 87, path_clean = clean_filename, directorio = tempdir())
