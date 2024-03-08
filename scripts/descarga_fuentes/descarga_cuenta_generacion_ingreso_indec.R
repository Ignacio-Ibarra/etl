serie_cgi <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_cgi_01_24.xls"

download.file(serie_cgi,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_FUENTES/raw/serie_cgi.xls"))

# 
# agregar_fuente(url = serie_cgi,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),path_raw = "serie_cgi.xls",
#                script = "descarga_cuenta_generacion_ingreso_indec.R",
#                nombre = "Valor agregado bruto e insumo de mano de obra por sector de actividad económica"
#                 )

actualizar_fuente(id_fuente = 39, fecha_descarga = Sys.Date())