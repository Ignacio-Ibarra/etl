# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


source("./scripts/utils/pip_world_bank_api.R")

POVERTY_LINE = 2.15
povline_str = sub('\\.','_',POVERTY_LINE)
ppp_version <- 2021


content <- pip_api.get_data(year = 'all', 
                            povline = POVERTY_LINE, 
                            country_code = 'all', 
                            fill_gaps = TRUE, 
                            reporting_level = 'all',
                            ppp_version = ppp_version)

df_raw <- content$data
url <- content$url

nombre <- glue::glue("Poverty and Inequality Platform - Poverty Line: {POVERTY_LINE} - PPP version: {ppp_version}")
institucion <- "Banco Mundial"

download_filename <- glue::glue("pip_povline_{povline_str}_ppp_version_{ppp_version}.csv")

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
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
