#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"
# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "https://www.cedlas.econo.unlp.edu.ar/wp/wp-content/uploads/2024_Act1_employment_LAC.xlsx"
download_filename <- basename(url)
destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

institucion = "Socio-Economic Database for Latin America and the Caribbean (SEDLAC)"

nombre = "Emplyment - SEDLAC"

# agregar_fuente_raw(url = url,
#                    nombre = "Emplyment - SEDLAC",
#                    institucion = "Socio-Economic Database for Latin America and the Caribbean (SEDLAC)",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "2023_Act2_employment_LAC.xlsx",
#                    script = code_name
# )


actualizar_fuente_raw(id_fuente = 115,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
