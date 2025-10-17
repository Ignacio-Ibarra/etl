#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

source("scripts/utils/st_lous_fed_api.R")

result_metadata <- FRED.get_series_metadata('GDPDEF')
result_data <- FRED.get_series_observations('GDPDEF')

periodicidad <- months(1)
fecha_ultima_actualizacion <- result_metadata$data$seriess$last_updated %>% as.Date(.)
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- result_data$url

df_raw <- result_data$data$observations

download_filename <- "gdp_deflator_quarterly.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

nombre <- result_metadata$data$seriess$title
institucion = "Federal Reserve Bank of St. Louis"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 462,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)