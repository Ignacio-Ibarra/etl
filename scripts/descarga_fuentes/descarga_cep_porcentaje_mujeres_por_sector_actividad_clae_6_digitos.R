# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2022-04-06")
fecha_actualizar <- "Sin informacion"


url <- "https://cdn.produccion.gob.ar/cdn-cep/datos-por-actividad/share-mujer/share_mujer_privado_mensual_por_clae6.csv"

download_filename <- "share_mujer_privado_mensual_por_clae6.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile)

nombre <- "Porcentaje de puestos ocupados por mujeres, por sector de actividad. Porcentaje de puestos de trabajo ocupados por mujeres en el sector privado, por mes y sector de actividad (CLAE a 6 dígitos)"

institucion = "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )



actualizar_fuente_raw(id_fuente = 315,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name,
                      api = F)




