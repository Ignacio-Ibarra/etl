
# emisiones_sector_global_1850-2014 co2  -----------

#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

# evolución historica CO2, año y valor  -----------

url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt"

filename <- "co2_mm_mlo.txt"

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{filename}"))


# agregar_fuente_raw(url = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt", institucion = "NOAA-NCDC", actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "co2_1958_2024.txt", 
#                    dir = tempdir(),
#                    script = "descarga_CO2_historico_NOAA_1958_2024.R",
#                    nombre = "Evolución CO2 1958-2024"
# )

actualizar_fuente_raw(id_fuente=161, fecha_actualizar = "Sin informacion", path_raw = filename, nombre = "Dioxido de carbono CO2 medición mensual de estación Mauna Loa, Estados Unidos." )

## > list.files(tempdir())




