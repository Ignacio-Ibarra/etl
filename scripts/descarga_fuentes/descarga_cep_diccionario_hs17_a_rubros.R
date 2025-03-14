# Este script carga un archivo de trabajo que no existe públicamente
# pero que es muy útil para clasificar las exportaciones en rubros o grandes rubros de INDEC. 
# Se elaboró en el CEP XXI mediante el uso de la base de datos no pública de INDEC. 


# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "None"

nombre <- "Diccionario de códigos NCM (8 dígitos) a rubros de INDEC"

institucion <- "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"

download_filename <- "hs17 a grandes rubros.xlsx"


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    directorio = "~/etl",
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 339,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)