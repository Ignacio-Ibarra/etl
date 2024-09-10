#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



get_raw_path <- function(codigo){
  prefix <- "/srv/shiny-server/static/etl-fuentes2/raw/"
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}



id_fuente <- 215
fuente_raw <- sprintf("R%sC0",id_fuente)



data_raw <- readxl::read_excel(get_raw_path(fuente_raw))

cols <- data_raw[12,] %>% as.matrix()

data_raw <- data_raw[13:nrow(data_raw),]

names(data_raw) <- cols

df_clean <- data_raw %>% rename(iso3 = `ISO3 Alpha-code`, anio = Year) %>% 
  select(all_of(c('iso3','anio',0:99,"100+"))) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  mutate(anio = as.integer(anio)) %>% 
  pivot_longer(-one_of(c("iso3","anio")),
               names_to = "edad",
               values_to = "expectativa_vida")

nombre_archivo_raw <- str_split_1(fuentes_raw() %>% 
                                    filter(codigo == fuente_raw) %>% 
                                    select(path_raw) %>% 
                                    pull(), pattern = "\\.")[1]

clean_filename <- glue::glue("{nombre_archivo_raw}_Estimaciones_CLEAN.parquet")

path_clean <- glue::glue("{tempdir()}/{clean_filename}")

df_clean %>% arrow::write_parquet(., sink = path_clean)

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

agregar_fuente_clean(id_fuente_raw = id_fuente,
                     path_clean = clean_filename,
                     dir = tempdir(),
                     nombre = "Life expectancy at exact age, both sexes (estimates)",
                     script = code_name)

actualizar_fuente_clean(id_fuente_clean = 33,
                        dir = tempdir())
