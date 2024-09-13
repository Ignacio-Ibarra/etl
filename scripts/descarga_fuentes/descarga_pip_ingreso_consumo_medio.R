#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

source("./scripts/utils/pip_world_bank_api.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

POVERTY_LINE = 2.15
povline_str = sub('\\.','_',POVERTY_LINE)


periodicidad <- months(6)
fecha_ultima_actualizacion <- as.Date("2024-03-26")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

content <- pip_api.get_data(year = 'all', 
                            povline = POVERTY_LINE, 
                            country_code = 'all', 
                            fill_gaps = TRUE, 
                            reporting_level = 'all')

df_raw <- content$data
url <- content$url


download_filename <- glue::glue("pip_povline_{povline_str}.csv")

df_raw %>% write_csv_fundar(., glue::glue("{tempdir()}/{download_filename}"))

# agregar_fuente_raw(nombre = glue::glue("Poverty and Inequality Platform - Poverty Line: {POVERTY_LINE}"),
#                    url = url,
#                    institucion = "Banco Mundial",
#                    actualizable = T,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 217,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename
)
