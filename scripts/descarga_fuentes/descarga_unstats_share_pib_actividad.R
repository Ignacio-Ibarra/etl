#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

source("./scripts/utils/unstats_ama_api.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(6)
fecha_ultima_actualizacion <- as.Date("2024-03-26")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

nombre_serie <- "Value Added by Economic Activity, Percentage Distribution (Shares)"

nombre_serie_normalized <- nombre_serie %>% janitor::make_clean_names()

m49_codes <- ama_api.get_countries()$data %>% 
  dplyr::filter(isCountry) %>% 
  pull(countryCode)

serieId <- ama_api.get_available_series()$data %>% 
  dplyr::filter(serieName == nombre_serie) %>% 
  pull(serieCode)

content <- ama_api.get_data(serieId = serieId,
                            m49_codes = m49_codes,
                            years = 1900:2024) # esto lo pongo así porque no programé los años disponibles

df_raw <- content$data
url <- content$url


download_filename <- glue::glue("UNSTATS_AMA_{nombre_serie_normalized}.csv")

df_raw %>% write_csv_fundar(., glue::glue("{tempdir()}/{download_filename}"))

# agregar_fuente_raw(nombre = glue::glue("National Accounts. Analysis of Main Aggregates (AMA). {nombre_serie}"),
#                    url = url,
#                    institucion = "United Nations Statistics Division (UNSD)",
#                    actualizable = T,
#                    fecha_actualizar = as.character(fecha_actualizar),
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 224,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name
)
