
# evolución historica CO2, año y valor  -----------

co2_1958_2024_url <- "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt"

download.file(url = co2_1958_2024_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/co2_1958_2024.txt"))


agregar_fuente_raw(url = "https://gml.noaa.gov/webdata/ccgg/trends/co2/co2_mm_mlo.txt", institucion = "NOAA-NCDC", actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "co2_1958_2024.txt", 
                   dir = tempdir(),
                   script = "descarga_CO2_historico_NOAA_1958_2024.R",
                   nombre = "Evolución CO2 1958-2024"
)

actualizar_fuente_raw(id_fuente=161,url = "https://nuevaurl",
                      fecha_descarga = Sys.Date())

## > list.files(tempdir())




