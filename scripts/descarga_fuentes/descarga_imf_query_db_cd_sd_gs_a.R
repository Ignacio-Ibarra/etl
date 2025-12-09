# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(rsdmx)


code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


source("scripts/utils/imf_api.R")


flowRef = "IMF.STA,BOP"
key=".CD_T+DB_T.SD+SDA+SDB+SDZ+GS..A"

result <- imf_get(flowRef = flowRef, key= key, startPeriod = 1976, endPeriod = 2024, format = "xml", detail = "full")


nombre_dataflow <- imf_get_dataflows()$data %>% 
  dplyr::filter(id == "BOP") %>% 
  pull(name)


nombre <- glue::glue("{nombre_dataflow}. API Query: flowRef: {flowRef}, key: {key}."
)

nombre_normalizado <- nombre %>% 
  janitor::make_clean_names()

url <- result$url

df_raw <- result$data

institucion <- "International Monetary Fund"


download_filename <- glue::glue("{nombre_normalizado}.csv")


destfile <- glue::glue("{tempdir()}/{download_filename}")


df_raw %>% argendataR::write_csv_fundar(., destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T)

actualizar_fuente_raw(id_fuente = 479,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)