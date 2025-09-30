#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


url <- "https://www.economicas.uba.ar/wp-content/uploads/2023/05/DT_10.pdf"

url_descarga <- "https://docs.google.com/spreadsheets/d/1INYvZtWk7GANhK9hBFJykO7yVausD__7jHjJR6oqMUA/export?format=csv&id=1INYvZtWk7GANhK9hBFJykO7yVausD__7jHjJR6oqMUA"

nombre <- "Documento de trabajo N° 10. El tamaño del gobierno en la economía argentina (1950 - 2021). Cuadro 11: Componentes de los egresos de gobierno como porcentaje del PIB de los países
seleccionados, pág 32"

institucion <- "Programa de Investigación de Cuentas Nacionales (PICNA). Facultad de Ciencias Económicas (UBA)"

download_filename <- "fce_uba_picna_dtnro10_kidyba.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url_descarga, destfile)

 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 427,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)