
url_oferta_demanda_globales_pib_2004_2023="https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_03_24.xls"

download.file(url = url_oferta_demanda_globales_pib_2004_2023, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/oferta_demanda_globales_pib_precios_corrientes_2004_2023.xls"))


# agrego la fuente
agregar_fuente_raw(url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_03_24.xls", 
                   institucion = "INDEC - Cuentas nacionales. Agregados macroeconómicos. PIB - Series trimestrales de oferta y demanda globales. Años 2004-2023", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "oferta_demanda_globales_pib_precios_corrientes_2004_2023.xls", 
                   dir = tempdir(),
                   script = "descarga_oferta_demanda_trimestrales_millones_precios_corrientes_pbi_2004_2023.R",
                   nombre = "Oferta y demanda globales. Valores trimestrales en millones de pesos a precios corrientes"
)

actualizar_fuente_raw(id_fuente=208 ,url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_oferta_demanda_03_24.xls",
                      fecha_descarga = Sys.Date())




