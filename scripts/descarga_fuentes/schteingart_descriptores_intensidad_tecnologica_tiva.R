#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


url <- "https://docs.google.com/spreadsheets/d/1WN9MfSPl-W2R_3eIKjmnS9NEunlXTHsJ/export?format=xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

 #subida manualmente a la sesión local y luego al server

download_filename <- "descriptores_intensidad_tecnologica_tiva.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

is_xlsx <- function(filename) {
  con <- file(filename, "rb")
  first_four_bytes <- readBin(con, "raw", n = 4)
  close(con)
  return(identical(first_four_bytes, as.raw(c(0x50, 0x4B, 0x03, 0x04))))
}

is_xlsx(destfile)
# agregar_fuente_raw(url = url,
#                    institucion = "Schteingart, Daniel",
#                    nombre = "Clasificación de Bienes y Servicios según intensidad tecnológica, elaboración propia (2023). Sin publicación",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 227,
                      fecha_actualizar = "Sin informacion",
                      path_raw = download_filename)
