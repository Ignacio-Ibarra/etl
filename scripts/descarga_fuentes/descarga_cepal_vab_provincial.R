code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(0)
fecha_ultima_actualizacion <- as.Date("2022-12-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://repositorio.cepal.org/server/api/core/bitstreams/42cdb69b-33be-439c-8bfd-c444dc644ed1/content"

# Desactivo la verificacion de SSL

download_filename <- "cepal_desagregacion_provincial_vab.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                     nombre = "Desagregación provincial del valor agregado bruto de la Argentina, base 2004",
#                     institucion = "Comisión Económica para América Latina y el Caribe (CEPAL) y el Ministerio de Economía de la Argentina, a través de la Secretaría de Planificación del Desarrollo y la Competitividad Federal, y el Centro de Estudios para la Producción (CEP XXI) de la Secretaría de Industria y Desarrollo Productivo",
#                     actualizable = F,
#                     path_raw = download_filename,
#                     script = code_name,
#                     fecha_actualizar = fecha_actualizar,
#                     api = F
# )

actualizar_fuente_raw(id_fuente = 221,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name
)
