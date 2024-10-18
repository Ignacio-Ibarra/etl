# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"


archivo <- "2dig_letra_cepcepal.xlsx"


# agregar_fuente_raw(url = "sin url",
#                    institucion = "Fundar",
#                    nombre = "Clasificación de sectores de actividad económica según letra de CAES. Elaboración propia en base Desagregación provincial del valor agregado bruto de la Argentina, base 2004 (CEPAL)",
#                    actualizable = F,
#                    path_raw = archivo,
#                    directorio = "~/etl",
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )


