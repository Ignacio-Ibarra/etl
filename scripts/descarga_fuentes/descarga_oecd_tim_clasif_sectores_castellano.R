# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


archivo <- "descriptores_oecd_tim_clasif_sectores_castellano.dta"


# agregar_fuente_raw(url = "sin url",
#                    institucion = "Fundar",
#                    nombre = "Códigos de actividad y descripción en español. Elaboración propia en base Trade in Employment (TiM) 2023 edition",
#                    actualizable = F,
#                    path_raw = archivo,
#                    directorio = "~/etl",
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )



actualizar_fuente_raw(id_fuente = 234,
                      fecha_actualizar = as.character(fecha_actualizar),
                      directorio = "~/etl",
                      path_raw = archivo)
