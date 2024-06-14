# evolución historica CO2, año y valor  -----------

co2_hist_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/paleo/icecore/antarctica/epica_domec/edc-co2-2008.xls"


download.file(url = co2_hist_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/co2_hist.xls"))


agregar_fuente_raw(url = "https://", institucion = "NOAA-NCDC", actualizable = T,
              fecha_descarga = Sys.Date(),
              path_raw = "co2_hist.xls", 
              dir = tempdir(),
              script = "descarga_CO2_historico_NOAA.R",
              nombre = "Evolución CO2 histórico"
               )

actualizar_fuente_raw(id_fuente=100,url = "https://nuevaurl",
                      fecha_descarga = Sys.Date())

## > list.files(tempdir())


