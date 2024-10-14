#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-09-30")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

url <- "https://dataverse.nl/api/access/datafile/356908"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

# download_filename <- "economic_transformation_database_2021.xlsx"

download_filename <- "etd-linked-series.rar"
destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb") # esta linea no funcionó se cargó manualmente, problema del server

archivo = "ETD_10SD_EASD.dta"
archive::archive_extract(destfile, dir = tempdir(), files = archivo)

# agregar_fuente_raw(url = url,
#                    institucion = "Groningen Growth and Development Centre",
#                    nombre = "GGDC/UNU-WIDER Economic Transformation Database (Linked to historical time series)",
#                    actualizable = T,
#                    path_raw = archivo,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 231,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = archivo)