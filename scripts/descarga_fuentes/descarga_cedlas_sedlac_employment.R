# SEDLAC - Employment

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/2023_Act2_employment_LAC.xlsx"
destfile <- glue::glue("{tempdir()}/2023_Act2_employment_LAC.xlsx")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Emplyment - SEDLAC",
#                    institucion = "Socio-Economic Database for Latin America and the Caribbean (SEDLAC)",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "2023_Act2_employment_LAC.xlsx",
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 115, dir = tempdir())