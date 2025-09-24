
# evolución temperature-anomaly  -----------

rm(list = ls())

url <- "https://ourworldindata.org/grapher/temperature-anomaly.csv?v=1&csvType=full&useColumnShortNames=true"


archivo <- glue::glue("{tempdir()}/temperature-anomaly-owid.csv")

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = archivo)

# agregar_fuente_raw(url = url,
#                    institucion = "OWID",api = F, 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    fecha_actualizar = "Sin informacion",
#                    path_raw = "temperature-anomaly-owid.csv",
#                    directorio = tempdir(),
#                    script = "descarga_R449C0.R",
#                    nombre = "Evolución de la anomalia de temperatura"
# )


actualizar_fuente_raw(id_fuente=450,url = url,script = "descarga_R450C0.R",
                      fecha_actualizar = "Sin informacion",
                      fecha_descarga = Sys.Date())
