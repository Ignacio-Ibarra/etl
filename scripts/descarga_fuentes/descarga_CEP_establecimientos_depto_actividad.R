#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-12-31")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

url <- "https://cdn.produccion.gob.ar/cdn-cep/establecimientos-productivos/Datos_por_departamento_y_actividad.csv"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "Datos_por_departamento_y_actividad.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = "Distribución geográfica de los establecimientos productivos. Datos de establecimientos por departamento y actividad"
institucion = "Ministerio de Economía. Secretaría de Industria. Dirección Nacional de Estudios para la Producción (CEP XXI) "

# agregar_fuente_raw(url = url, 
#                    nombre = nombre,
#                    institucion = institucion, 
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 240,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)