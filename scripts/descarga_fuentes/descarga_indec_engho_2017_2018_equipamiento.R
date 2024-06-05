# ENGHO 2017 - 2018 (Equipamiento)
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

nombre <- "engho2018_equipamiento"

url <- glue::glue("https://www.indec.gob.ar/ftp/cuadros/menusuperior/engho/{nombre}.zip")

destfile <- glue::glue("{tempdir()}/{nombre}.zip")

outfolder <- glue::glue("{tempdir()}")

# Descargar el archivo
download.file(url, destfile, mode = "wb")

# Descomprimir el archivo
unzip(destfile, exdir = outfolder)

path_raw <- glue::glue("{nombre}.txt")

nombre_fuente <- "Encuesta Nacional de Gastos de los Hogares 2017-2018. Equipamiento"

# agregar_fuente_raw(url = url,
#                    nombre = nombre_fuente,
#                    institucion = "INDEC",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = path_raw,
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 106, dir = tempdir())