# oferta y demanda global trimestral INDEC cuentas nacionales  -----------
oyd_cn_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_12_23.xls"  

download.file(url = oyd_cn_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_FUENTES/raw/sh_oferta_demanda_12_23.xls"))


# agregar_fuente(url = oyd_cn_indec_url,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),path_raw = "sh_oferta_demanda_12_23.xls",
#                script = "descarga_oferta_demanda_indec.R",
#                nombre = "Series trimestrales de oferta y demanda globales"
#                 )

actualizar_fuente(id_fuente = 38, fecha_actualizar = Sys.Date())