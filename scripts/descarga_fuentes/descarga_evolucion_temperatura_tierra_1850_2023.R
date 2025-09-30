# evolución temperatura tierra 1850-2023  -----------
rm(list = ls())
temp_tierra_1850_2023_url <- "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt"

archivo <- "temp_tierra_1850.txt"

download.file(url = temp_tierra_1850_2023_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_raw(url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt", 
#                    institucion = "National Science Foundation - NCAR", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "temp_tierra_1850_2023.txt", 
#                    dir = tempdir(),
#                    script = "descarga_evolucion_temperatura_tierra_1850_2023.R",
#                    nombre = "Evolución temperatura tierra 1850-2023"
# )

comparar_archivos( glue::glue("{tempdir()}/{archivo}"), get_fuente_path("R122C0"))

actualizar_fuente_raw(id_fuente=122, nombre = "Anomalias mensuales de temperatura de la superficie de la tierra relativas al promedio de Ene 1951 - Dic 1980",
                      url = temp_tierra_1850_2023_url, institucion = "Berkeley Earth",
                      path_raw = archivo,
                      fecha_actualizar = "Sin informacion",
                      fecha_descarga = Sys.Date())

list.files(tempdir())
limpiar_temps()
