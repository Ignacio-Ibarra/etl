# CEPED - Distribución Funcional Argentina

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

path_raw <- "ceped_dist_funcional_arg.xlsx"

# Es necesario buscar la URL haciendo la consulta con el browser o hacerlo dinámicamente con Selenium
temp_url <- "https://ceped-data.shinyapps.io/ceped-data/_w_79e2be4c/session/090a940e9504cdc8aa2989e661fb1d9c/download/df-downloadTable?w=79e2be4c"

destfile <- glue::glue("{tempdir()}/{path_raw}")

outfolder <- glue::glue("{tempdir()}")

# Descargar el archivo
download.file(temp_url, destfile, mode = "wb")

nombre_fuente <- "Distribución Funcional Argentina"

# agregar_fuente_raw(url = "https://ceped-data.shinyapps.io/ceped-data/",
#                    nombre = nombre_fuente,
#                    institucion = "Centro de Estudios sobre Población, Empleo y Desarrollo (CEPED)",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = path_raw,
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 210, dir = tempdir())