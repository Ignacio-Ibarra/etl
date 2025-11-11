
# -----------

#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

filename <- "ingei_desagregacion_provincial.xlsx"
url <- "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2022.xlsx"

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{filename}"))

# agrego la fuente
# agregar_fuente_raw(url = "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx", 
#                    institucion = "Ministerio Ambiente - Informe Bienal de Actualización", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emis_arg_prov_2010_2018.xlsx", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_provincia_2010_2018.R",
#                    nombre = "Emisiones por Provincia. Año 2010-2018"
# )

actualizar_fuente_raw(id_fuente=157 ,url = url, path_raw = filename, script = code_name,
                      nombre =  "Desagregacion provincial - Inventario nacional de gases de efecto invernadero",
                      fecha_actualizar = "Sin informacion")

#list.files(tempdir())

#fuentes() %>% 
#  view()



