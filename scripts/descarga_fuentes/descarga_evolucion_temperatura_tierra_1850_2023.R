# evolución temperatura tierra 1850-2023  -----------

temp_tierra_1850_2023_url <- "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt"

download.file(url = temp_tierra_1850_2023_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/temp_tierra_1850_2023.txt"))

agregar_fuente_raw(url = "https://berkeley-earth-temperature.s3.us-west-1.amazonaws.com/Global/Complete_TAVG_complete.txt", 
                   institucion = "National Science Foundation - NCAR", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "temp_tierra_1850_2023.txt", 
                   dir = tempdir(),
                   script = "descarga_evolucion_temperatura_tierra_1850_2023.R",
                   nombre = "Evolución temperatura tierra 1850-2023"
)

actualizar_fuente_raw(id_fuente=122,url = "https://www.metoffice.gov.uk/hadobs/hadsst4/data/csv/HadSST.4.0.1.0_monthly_GLOBE.csv",
                      fecha_descarga = Sys.Date())

list.files(tempdir())
limpiar_temps()
