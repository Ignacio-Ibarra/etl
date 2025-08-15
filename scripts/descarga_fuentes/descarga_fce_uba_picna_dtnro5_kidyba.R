#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


url <- "https://www.economicas.uba.ar/wp-content/uploads/2021/06/picna-5.pdf"

url_descarga <- "http://www.economicas.uba.ar/wp-content/uploads/2021/06/anexo-estadistico-2021.xlsx"

nombre <- "Kidyba, Susana y Suárez, Luis (2021). Documento de trabajo N° 5. Aplicación de los Índices Encadenados al empalme de series Argentina (1935-2020). Anexo estadístico"

institucion <- "Programa de Investigación de Cuentas Nacionales (PICNA). Facultad de Ciencias Económicas (UBA)"

download_filename <- "fce_uba_picna_dtnro5_kidyba.csv"

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

actualizar_fuente_raw(id_fuente = 429,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)