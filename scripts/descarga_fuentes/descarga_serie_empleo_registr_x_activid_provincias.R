
#limpio la memoria
#rm( list=ls())  #Borro todos los objetos
#gc()   #Garbage Collection

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-24")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://www.argentina.gob.ar/sites/default/files/provinciales_serie_empleo_trimestral_4dig_actualizado_120523.xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "provinciales_serie_empleo_trimestral_4dig_actualizado_120523.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# Localizacion del empleo

# agregar_fuente_raw(url = url,
#                    institucion = "OEDE",
#                    nombre = "Serie de empleo registrado por rama de actividad y Provincia",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 247,
                      nombre = "Serie de empleo registrado por rama de actividad y Provincia",
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)


