
url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vR0-iaTaOeaecY55AsVMJdKBw5xyHe0Cch1EtU3rp0oscnDkCB8b8kr4RXiJKoT7A/pub?output=xlsx"

archivo <- "precios_salarios_ocupaciones_fyns.xlsx"

download.file(url, mode = "wb",
              destfile = glue::glue("{tempdir()}/{archivo}"))

agregar_fuente_raw(
  url = url,
  nombre = "Precios, salarios y ocupaciones",
  institucion = "Fundación Norte y Sur",
  actualizable = F,
  fecha_descarga = Sys.Date(),
  fecha_actualizar = NULL,
  path_raw = archivo,
  script = "descargar_precios_salarios_ocup_fnys.R"
)

actualizar_fuente_raw(id_fuente = 120)