# cuentas nacionales fundacion norte y sur   -----------
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004

link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls"
ruta <- "data/_FUENTES/raw/cuentas-nacionales-fundacion-norte-y-sur.xlsx"

download.file(link,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = ruta)


# agregar_fuente_raw(url = link, nombre = "Cuentas Nacionales",
#                institucion = "Fundación Norte y Sur", actualizable = F,
#                path_raw = "cuentas-nacionales-fundacion-norte-y-sur.xlsx",
#                script = "descarga_cuentas-nacionales-fund-norte-y-sur.R",
#                fecha_descarga = Sys.Date())

actualizar_fuente(id = 36, fecha_descarga = Sys.Date())