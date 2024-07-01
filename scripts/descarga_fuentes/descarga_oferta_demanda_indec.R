# oferta y demanda global trimestral INDEC cuentas nacionales  -----------
oyd_cn_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_06_24.xls"  

download.file(url = oyd_cn_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/sh_oferta_demanda.xls"))


# agregar_fuente_raw(url = oyd_cn_indec_url,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),path_raw = "sh_oferta_demanda.xls",
#                script = "descarga_oferta_demanda_indec.R",
#                nombre = "Series trimestrales de oferta y demanda globales"
#                 )

actualizar_fuente_raw(id_fuente = 38)

