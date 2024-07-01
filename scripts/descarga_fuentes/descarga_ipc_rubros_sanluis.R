url <- "https://www.ieric.org.ar/wp-content/uploads/2024/06/IPC-Prov-San-Luis.xlsx"

archivo <- glue::glue("ipc_rubros_2005_{lubridate::year(Sys.Date())}_san_luis.xlsx")

download.file(url, destfile = glue::glue("{tempdir()}/{archivo}"), mode = "wb")


# agregar_fuente_raw(
#   url = url,
#   nombre = "Índice de Precios al Consumidor San Luis. Nivel General y Capítulos de la Canasta. Desde Octubre de 2005 en adelante. Base 2003 = 100",
#   institucion = "Dirección de Estadísticas y Censos de la provincia de San Luis",
#   actualizable = T,
#   fecha_descarga = Sys.Date(),
#   path_raw = archivo,
#   script = "scripts/descarga_fuentes/descarga_ipc_rubros_sanluis.R",
#   api = F
# )

actualizar_fuente_raw(id_fuente = 156, url = url)

