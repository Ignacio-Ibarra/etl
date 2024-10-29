#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-05-02")
fecha_actualizar <- "Sin informacion"

titulo <- "Cantidad y porcentaje de hogares en viviendas particulares con al menos un indicador de Necesidades Básicas Insatisfechas"

download_filename <- "_tmp_97384141.xlsX" # Cargado manualmente desde local. 

nombre = glue::glue("Censo Nacional de Población, Hogares y Viviendas 2022. Resultados definitivos. {titulo}. (Procesado con Redatam 7, CEPAL/CELADE, 2024-10-28).")
institucion = "INDEC"

# agregar_fuente_raw(url = "https://redatam.indec.gob.ar/binarg/RpWebEngine.exe/Portal?BASE=CPV2022&lang=ESP",
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    directorio = "~/etl",
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 249,
                      nombre = nombre,
                      directorio = "~/etl",
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)
