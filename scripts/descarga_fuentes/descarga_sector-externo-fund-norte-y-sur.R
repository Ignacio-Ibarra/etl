#sector externo fundacion norte y sur   -----------


link <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQIpRVAAMYpexDE6dtJqDGGtK-mRYcXXRMpAZLM6xAzy1ICloxn6GkStlPvNFe1yA/pub?output=xls"
ruta <- "data/_FUENTES/raw/sector-externo-fundacion-norte-y-sur.xlsx"

download.file(link,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = ruta)


# agregar_fuente_raw(url = link, nombre = "Sector Externo",
#                institucion = "Fundación Norte y Sur", actualizable = F, 
#                dir = "data/_FUENTES/raw/",
#                path_raw = "sector-externo-fundacion-norte-y-sur.xlsx",
#                script = "descarga_sector-externo-fund-norte-y-sur.R",
#                fecha_descarga = Sys.Date())

actualizar_fuente(id = 43, fecha_descarga = Sys.Date())