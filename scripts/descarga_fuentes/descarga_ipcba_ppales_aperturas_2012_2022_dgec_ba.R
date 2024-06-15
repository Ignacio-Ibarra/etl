url <- "https://www.estadisticaciudad.gob.ar/eyc/wp-content/uploads/2023/05/IPCBA_base_2021100-Principales_aperturas_empalme.xlsx"

archivo <- "ipc_base2021_ppales_aperturas_empalme.xlsx"

download.file(url = url, destfile = glue::glue("{tempdir()}/{archivo}"), mode = "wb")


agregar_fuente_raw(
  url = url,
  nombre = "IPCBA (base 2021 = 100) según principales aperturas. Índice mensual empalmado con la serie anterior (base julio 2011 – junio 2012 = 100). Ciudad de Buenos Aires. Julio de 2012 / febrero de 2022",
  institucion = "Dirección General de Estadística y Censos - DGEC - GCBA",
  fecha_descarga = Sys.Date(),
  path_raw = archivo,actualizable = F, 
  script = "scripts/descarga_fuentes/descarga_ipcba_ppales_aperturas_2012_2022_dgec_ba.R"
)

actualizar_fuente_raw(135)