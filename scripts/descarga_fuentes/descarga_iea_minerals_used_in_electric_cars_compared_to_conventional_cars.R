code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2021-05-05")
fecha_actualizar <- "Sin informacion"


report_url <- "https://www.iea.org/data-and-statistics/charts/minerals-used-in-electric-cars-compared-to-conventional-cars"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "minerals-used-in-electric-cars-compared-to-conventional-cars.csv"


nombre = "Minerals used in electric cars compared to conventional cars."
institucion = "International Energy Agency (IEA)"

# agregar_fuente_raw(url = report_url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    directorio = "~/etl",
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 281,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      directorio = "~/etl",
                      path_raw = download_filename,
                      script = code_name)
