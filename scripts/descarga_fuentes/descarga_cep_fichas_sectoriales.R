# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2023-12-31")
fecha_actualizar <- "Sin informacion"

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "https://cdn.produccion.gob.ar/cdn-cep/fichas-sectoriales/Fichas-Sectoriales-FINAL.csv"

download_filename <- "Fichas-Sectoriales-FINAL.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = "Fichas Sectoriales"

institucion = "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"


# agregar_fuente_raw(url = url,
#                    nombre = "Fichas Sectoriales CEP XXI",
#                    institucion = "Ministerio de Desarrollo Productivo. Unidad Gabinete de Asesores. Dirección Nacional de Estudios para la Producción (CEP XXI)",
#                    actualizable = F,
#                    fecha_actualizar = fecha_actualizar,
#                    path_raw = download_filename,
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 229,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name,
                      api = F)