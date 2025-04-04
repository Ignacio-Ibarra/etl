# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-12-16")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad  


source("scripts/utils/ricyt_api.R")

indicator_id <- "PUB_AREA"

indicador_seleccionado <- RICYT.get_indicators() %>% 
  dplyr::filter(id_indicator == indicator_id)


resultado <- RICYT.get_indicator_data(id_indicator = indicator_id) 

url <- resultado$url_consulta

response <- resultado$response

nombre <- indicador_seleccionado$indicator

institucion <- "Red Iberoamericana de Indicadores de Ciencia y Tecnología (RICYT)"

download_filename <- glue::glue("RICYT_{indicator_id}.json")

destfile <- glue::glue("{tempdir()}/{download_filename}")

jsonlite::write_json(response, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 364,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)