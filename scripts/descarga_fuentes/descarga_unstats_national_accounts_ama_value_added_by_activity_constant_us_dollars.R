# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("./scripts/utils/unstats_ama_api.R")


serieId <- 24

metadatos <- ama_api.get_available_series()$data %>% 
  dplyr::filter(serieCode == serieId)

nombre_archivo <- glue::glue("National Accounts. Analysis of Main Aggregates (AMA). {metadatos$serieName}")

nombre_archivo_normalized <- nombre_archivo %>% janitor::make_clean_names()

download_filename <- glue::glue("UNSTATS_AMA_{nombre_archivo_normalized}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

m49_codes <- ama_api.get_countries()$data %>% 
  dplyr::filter(isCountry) %>% 
  pull(countryCode) %>% 
  c(1, .)

result <- ama_api.get_data(serieId = serieId,
                           m49_codes = m49_codes,
                           years = 1900:2024)


df_raw <- result$data

df_raw %>% argendataR::write_csv_fundar(., destfile)

url <- result$url

institucion <- "United Nations Statistics Division"

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 464,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
