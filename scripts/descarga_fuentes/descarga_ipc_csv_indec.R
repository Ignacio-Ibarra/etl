url <- "https://www.indec.gob.ar/ftp/cuadros/economia/serie_ipc_divisiones.csv"

archivo <- "ipc_total_regiones_divisiones_indec.csv"

ipc_indec <- readr::read_csv2(url, locale = locale(encoding = "latin1"))


write_csv_fundar(x = ipc_indec, file = glue::glue("{tempdir()}/{archivo}"))

# agregar_fuente_raw(
#   url = url,
#   nombre = "Indice de precios al consumidor (IPC) por regiones según divisiones, categorías, bienes y servicios",
#   institucion = "INDEC",
#   actualizable = T,
#   fecha_descarga = Sys.Date(),
#   fecha_actualizar = Sys.Date()+months(1),
#   path_raw = archivo,
#   script = "descarga_ipc_csv_indec.R"
# )

actualizar_fuente_raw(id_fuente = 127)
