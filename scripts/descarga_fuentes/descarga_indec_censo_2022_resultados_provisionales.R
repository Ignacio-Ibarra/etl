# Censo Nacional 2022 - Resultados provisionales
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- url <- "https://www.censo.gob.ar/wp-content/uploads/2023/01/cnphv2022_resultados_provisionales-.xlsx"
destfile <- glue::glue("{tempdir()}/cnphv2022_resultados_provisionales.xlsx")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Censo 2022 - Resultados provisionales",
#                    institucion = "INDEC",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "cnphv2022_resultados_provisionales.xlsx",
#                    script = code_name
# )

actualizar_fuente_raw(
  id = "R99C0",
  url = url,
  actualizable = T,
  path_raw = "cnphv2022_resultados_provisionales.xlsx",
  fecha_descarga = Sys.Date()
)