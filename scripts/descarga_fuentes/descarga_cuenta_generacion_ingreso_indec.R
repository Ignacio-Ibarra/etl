serie_cgi <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_04_24.xls"

download.file(serie_cgi,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/serie_cgi.xls"))


# agregar_fuente_raw(url = serie_cgi,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),
#                path_raw = "serie_cgi.xls",
#                script = "descarga_cuenta_generacion_ingreso_indec.R",
#                nombre = "Valor agregado bruto e insumo de mano de obra por sector de actividad económica"
#                 )

actualizar_fuente_raw(id_fuente = 35, fecha_descarga = Sys.Date(), path_raw = "serie_cgi.xls", url = serie_cgi )
