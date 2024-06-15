################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

url <- "https://www.indec.gob.ar/ftp/nuevaweb/cuadros/17/pbi_80-05_cuadro1.xls"


archivo <- "pbi_vab_sectorial_1980_2005_trim_pb_precios93.xls"


download.file(url, destfile = glue::glue("{tempdir()}/{archivo}"), mode = "wb")

# agregar_fuente_raw(
#   url = url,
#   nombre = "Producto Interno Bruto a precios de mercado y Valor Agregado Bruto, por sector económico, a precios básicos. En millones de pesos, a precios de 1993",
#   institucion = "INDEC",
#   actualizable = F,fecha_descarga = Sys.Date(), fecha_actualizar = NULL,
#   path_raw = archivo,
#   script = "descarga_pbi_1980_2005_trim_indec.R",
#   api = F
# )


actualizar_fuente_raw(133)