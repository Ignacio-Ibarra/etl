

# sea level rise  -----------

rm(list = ls())

url <- "https://ourworldindata.org/grapher/sea-level.csv?v=1&csvType=full&useColumnShortNames=true"


archivo <- glue::glue("{tempdir()}/sea_level_rise.csv")

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = archivo)

agregar_fuente_raw(url = url,
                   institucion = "OWID",api = F,
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   fecha_actualizar = "Sin informacion",
                   path_raw = "sea_level_rise.csv",
                   directorio = tempdir(),
                   script = "descarga_R451C0.R",
                   nombre = "Evolución del aumento del nivel del mar"
)


actualizar_fuente_raw(id_fuente=451,url = url,script = "descarga_R450C0.R",
                      fecha_actualizar = "Sin informacion",
                      fecha_descarga = Sys.Date())
