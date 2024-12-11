code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "https://www.gold.org/download/file/7593/Gold-Mining-Production-Volumes-Data-2024.xlsx"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "Gold-Mining-Production-Volumes-Data-2024.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

# download.file(url, destfile = destfile, mode = "wb")

nombre = "Gold Mining Production Volumes (Data Release 2024)"
institucion = "World Gold Council"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    directorio = "~/etl",
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 294,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)