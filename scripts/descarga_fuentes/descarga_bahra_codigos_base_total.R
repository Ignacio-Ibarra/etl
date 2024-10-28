#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-05-02")
fecha_actualizar <- "Sin informacion"


titulo <- "Base de Asentamientos Humanos de la República Argentina"

download_filename <- "base_total.csv"

url <- "http://www.bahra.gob.ar/descargas/archivos/base_total/base_total.csv.7z"

nombre = titulo
institucion = "Instituo Geográfico Nacional (IGN)"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 244,
                      nombre = nombre, 
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      directorio = "~/etl",
                      script = code_name, 
                      api = T)