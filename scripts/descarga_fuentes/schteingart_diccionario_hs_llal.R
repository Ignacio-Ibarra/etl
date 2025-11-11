#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


url <- "https://docs.google.com/spreadsheets/d/15HNMkpO9x4Ymo_YZWYlkD1_wryEYcdxo/export?format=xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

#subida manualmente a la sesión local y luego al server

download_filename <- "diccionario_hs_llal.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

# agregar_fuente_raw(url = url,
#                    institucion = "Schteingart, Daniel",
#                    nombre = "Diccionario de conversión de códigos HS02 a categorías tecnológicas de Lall (2000). Sin publicación",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 458,
                      fecha_actualizar = "Sin informacion",
                      path_raw = download_filename)