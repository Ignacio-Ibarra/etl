#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-18")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://www.indec.gob.ar/ftp/cuadros/economia/sh_VBP_VAB_09_24.xls"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "sh_VBP_VAB_09_24.xls"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = "INDEC",
#                    nombre = "Cuentas Nacionales. Agregados Macroeconómicos (PIB). Series por sector de actividad económica: valor bruto de producción y valor agregado bruto, por trimestre",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 223,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)