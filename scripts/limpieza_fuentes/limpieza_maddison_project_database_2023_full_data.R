#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


id_fuente <- 219
fuente_raw <- sprintf("R%sC0",id_fuente)

sheet_name = "Full data"


regiones_MDP <- c('East Asia' = "Asia Oriental (Maddison Project Database)",
              'Eastern Europe' = "Europa Oriental (Maddison Project Database)",
               'Latin America' = "América Latina (Maddison Project Database)",
               'Middle East and North Africa' = "Medio Oriente y África del Norte (Maddison Project Database)",
               'South and South East Asia' = "Asia del Sur y Sudeste (Maddison Project Database)",
               'Sub Saharan Africa' = "África Subsahariana (Maddison Project Database)",
               'Western Europe' = "Europa Occidental (Maddison Project Database)",
               'Western Offshoots' = "Ramificaciones de Occidente (Maddison Project Database)",
               'World' = "Mundo")


geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, nivel_agregacion) 

df_clean <- readxl::read_excel(get_raw_path(fuente_raw), sheet = sheet_name) %>% 
  select(iso3 = countrycode, region, anio = year, gdppc, pop) %>%
  mutate(region = regiones_MDP[region],
         gdppc = as.numeric(gdppc),
         pop = as.numeric(pop) * 1000,
         pib = gdppc * pop) %>% 
  left_join(geonomenclador, by = join_by(iso3))
  


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
#                      nombre = "Maddison Project Database (2023) - Solapa: Full Data",
#                      script = code_name)

actualizar_fuente_clean(id_fuente_clean = 90, path_clean = clean_filename, directorio = tempdir())