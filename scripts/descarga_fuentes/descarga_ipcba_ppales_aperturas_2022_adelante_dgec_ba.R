url <- "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2024/04/IPCBA_base_2021100-Principales_aperturas_indices.xlsx"

archivo <- "ipc_base2021_ppales_aperturas_2022_adelante_dgec_ba.xlsx"

download.file(url, glue::glue("{tempdir()}/{archivo}"), mode = "wb")

agregar_fuente_raw(
  url = url,
  nombre = "IPCBA (base 2021 = 100) según principales aperturas. Índice mensual. Ciudad de Buenos Aires. Febrero de 2022 / mayo de 2024",
  institucion = "Dirección General de Estadística y Censos - DGEC - GCBA",
  fecha_descarga = Sys.Date(),
  actualizable = T,
  fecha_actualizar = Sys.Date() + 1,
  path_raw = archivo,
  script = "scripts/descarga_fuentes/descarga_ipcba_ppales_aperturas_2022_adelante_dgec_ba.R"
)

actualizar_fuente_raw(136, nombre = glue::glue("IPCBA (base 2021 = 100) según principales aperturas. Índice mensual. Ciudad de Buenos Aires. Febrero de 2022 / {Sys.Date()-months(1)}"))
