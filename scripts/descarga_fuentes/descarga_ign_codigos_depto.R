#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-05-02")
fecha_actualizar <- "Sin informacion"


titulo <- "Lista de las entidades que representan la división político administrativa de segundo orden de la República Argentina, incluye partidos y comunas. Formato CSV"

download_filename <- "igndepartamento.csv"

url <- "http://www.ign.gob.ar/descargas/geodatos/CSV/igndepartamento.csv"

nombre = titulo
institucion = "Instituo Geográfico Nacional (IGN)"


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    directorio = "~/etl",
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 248,
                      nombre = nombre, 
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      directorio = "~/etl",
                      script = code_name)