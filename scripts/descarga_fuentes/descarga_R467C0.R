rm(list = ls())

url <- "https://cammesaweb.cammesa.com/download/potencia-instalada/?wpdmdl=41444"

archivo <- "cammesa_potencia_instalada.xlsx"

download.file(url = url, destfile = glue::glue("{tempdir()}/{archivo}"), mode = "wb")


agregar_fuente_raw(
  url = url,
  nombre = "Potencia de generación eléctrica instalada Argentina",
  institucion =  "CAMMESA",
  actualizable = T,
  path_raw = archivo,
  directorio = tempdir(),
  fecha_actualizar = "Sin informacion",
  script = "descarga_R467C0.R",
  api = F
)
# 
# 
actualizar_fuente_raw(467, script = "descarga_R467C0.R")