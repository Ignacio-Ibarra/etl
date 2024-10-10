#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


url <- "https://stats.oecd.org/wbos/fileview2.aspx?IDFile=6e26de1c-ec5c-4bab-9c98-ecd3b5e74da3"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "oecd_icio_tables.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = "OECD",
#                    nombre = "Inter-Country Input-Output tables. ReadMe",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 226,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)