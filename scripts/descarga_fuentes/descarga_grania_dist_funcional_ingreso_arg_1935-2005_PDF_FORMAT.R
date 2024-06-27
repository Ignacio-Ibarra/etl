#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "http://bibliotecadigital.econ.uba.ar/download/docin/docin_ceped_d_008.pdf"
destfile <- glue::glue("{tempdir()}/docin_ceped_d_008.pdf")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Distribucion Funcional del Ingreso en la Argentina (1935 - 2005)",
#                    institucion = "Graña, Juan M. (2007) - CEPED",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "docin_ceped_d_008.pdf",
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 211, dir = tempdir())