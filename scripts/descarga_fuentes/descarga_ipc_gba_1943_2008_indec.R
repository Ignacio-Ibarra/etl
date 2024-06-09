url <- "https://www.indec.gob.ar/ftp/nuevaweb/cuadros/10/sh_ipc_2008.xls"

archivo <- "ipc_gba_1943_2008.xls"

download.file(url = url,
              mode = "wb",
              destfile = glue::glue("{tempdir()}/{archivo}"))

agregar_fuente_raw(
  url = url,
  nombre = "Índice de Precios al Consumidor (IPC) GBA desde 1943 en adelante (empalme de las series base 1943, 1960, 1974, 1988 y 1999 con la serie base abril 2008=100), nivel general y capítulos de la canasta",
  institucion = "INDEC",
  actualizable = T,
  fecha_descarga = Sys.Date(),
  fecha_actualizar = Sys.Date()+months(1),
  path_raw = archivo,
  script = "descarga_ipc_gba_1943_2008_indec.R"
)

actualizar_fuente_raw(id_fuente = 128)