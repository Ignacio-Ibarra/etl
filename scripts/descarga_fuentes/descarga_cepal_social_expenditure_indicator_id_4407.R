# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2025-01-01")
fecha_actualizar <- "Sin informacion"

source("scripts/utils/cepalstat_api.R")

indicator_id = 4407

result <- cepalstat_api.get_indicator_cube(indicator_id = indicator_id) 

url <- result$footer$query

metadata <- result$body$metadata

metadata$indicator_id

download_filename <- glue::glue("cepalstat_indicator_id_{indicator_id}.json")

destfile <- glue::glue("{tempdir()}/{download_filename}")

nombre <- metadata$indicator_name

institucion = result$body$credits[3,2]

result %>% jsonlite::write_json(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 404,
                      nombre = nombre, 
                      institucion = institucion, 
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name)