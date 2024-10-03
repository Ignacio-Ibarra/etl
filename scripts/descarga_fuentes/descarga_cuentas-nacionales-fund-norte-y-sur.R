#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# periodicidad <- months(3)
# fecha_ultima_actualizacion <- as.Date("2019-09-30") 
# fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls"

# Desactivo la verificacion de SSL
options(download.file.method="libcurl"
        # , download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
)

download_filename <- "cuentas-nacionales-fundacion-norte-y-sur.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = link, nombre = "Cuentas Nacionales",
#                institucion = "Fundación Norte y Sur", actualizable = F,
#                path_raw = "cuentas-nacionales-fundacion-norte-y-sur.xlsx",
#                script = "descarga_cuentas-nacionales-fund-norte-y-sur.R",
#                fecha_descarga = Sys.Date())


actualizar_fuente_raw(id_fuente = 36,
                      fecha_actualizar = "Sin informacion",
                      path_raw = download_filename)
