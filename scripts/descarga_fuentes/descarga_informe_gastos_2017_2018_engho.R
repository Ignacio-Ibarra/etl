url <- "https://www.indec.gob.ar/ftp/cuadros/sociedad/cuadros_engho_2017_2018_informe_gastos.xls"

archivo <- "cuadros_engho_2017_2018_informe_gastos.xls"

ruta <- glue::glue("{tempdir()}/{archivo}")

download.file(url, destfile = ruta, mode = "wb")

# agregar_fuente_raw(
#   url = url,
#   nombre = "cuadros de la ENGHO 2017-2018. Informe de gastos",
#   institucion = "INDEC",
#   fecha_descarga = Sys.Date(),
#   actualizable = F,
#   path_raw = archivo,
#   script = "scripts/descarga_fuentes/descarga_informe_gastos_2017_2018_engho.R",
#   api = F
# )

actualizar_fuente_raw(id_fuente = 134)


