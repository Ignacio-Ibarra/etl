url <- "http://www.estadistica.sanluis.gov.ar/wp-content/uploads/SERIE-1.xlsx"


archivo <- "ipc_san_luis.xlsx"

download.file(url, mode = "wb",
              destfile = glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_raw(
#   url = url,
#   nombre = "Indice de precios al consumidor (IPC) - San Luis",
#   institucion = "Dirección de Estadísticas y Censos de la provincia de San Luis",
#   actualizable = T,
#   fecha_descarga = Sys.Date(),
#   fecha_actualizar = NULL,
#   path_raw = archivo,
#   script = "descarga_ipc_san_luis.R"
# )

actualizar_fuente_raw(id_fuente = 124)
