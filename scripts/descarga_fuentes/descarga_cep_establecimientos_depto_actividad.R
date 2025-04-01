# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2023-12-31")
fecha_actualizar <- "Sin informacion"

url <- "https://cdn.produccion.gob.ar/cdn-cep/establecimientos-productivos/Datos_por_departamento_y_actividad.csv"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "Datos_por_departamento_y_actividad.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = "Distribución geográfica de los establecimientos productivos. Datos de establecimientos por departamento y actividad"
institucion = "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"

# agregar_fuente_raw(url = url, 
#                    nombre = nombre,
#                    institucion = institucion, 
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 240,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name,
                      api = F)