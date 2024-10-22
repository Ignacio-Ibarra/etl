#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2023-05-02")
fecha_actualizar <- "Sin informacion"

titulo <- "Códigos y nombres de departamento"

download_filename <- "codigos.json" # Cargado manualmente desde local. 

nombre = glue::glue("Censo Nacional de Población, Hogares y Viviendas 2022. Resultados definitivos. {titulo}")
institucion = "INDEC"

agregar_fuente_raw(url = "https://geocubo.indec.gob.ar/?indicator_id=93&members=108,34",
                   nombre = nombre,
                   institucion = institucion,
                   actualizable = F,
                   path_raw = download_filename,
                   directorio = "~/etl",
                   script = code_name,
                   fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 241,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename)