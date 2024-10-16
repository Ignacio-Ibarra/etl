
url="https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_03_24.xls"

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/series_x_sector_vbp_vab__bas_corr_2004_2023.xls"))

# # agrego la fuente
# agregar_fuente_raw(url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_03_24.xls", 
#                    institucion = "INDEC - Cuentas nacionales. Series por sector de actividad económica: valor bruto de producción y valor agregado bruto. Años 2004-2023, por trimestre", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "series_x_sector_vbp_vab__bas_corr_2004_2023.xls", 
#                    dir = tempdir(),
#                    script = "descarga_series_x_sector_vbp_vab__bas_corr_2004_2023.R",
#                    nombre = "Valor Agregado Bruto a precios básicos por rama de actividad económica. Valores anuales en millones de pesos a precios corrientes"
# )

actualizar_fuente_raw(id_fuente=212 ,url = "https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_03_24.xls",
                      fecha_descarga = Sys.Date(),
                      fecha_actualizar = "Sin informacion")

