# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

url <- "https://docs.anac.gob.ar/index.php/s/4ptegdXanm2rWJG/download"

nombre_archivo <- "Tablas de Movimientos y Pasajeros"

institucion <- "Dirección de Estudios de Mercado y Estadísticas. Dirección Nacional de Transporte Aéreo. Administración Nacional de Aviación Civil (ANAC)"

download_filename <- "series-historicas-2001-2022.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 486,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)