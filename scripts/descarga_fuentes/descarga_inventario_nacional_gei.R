
#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


url <- "https://inventariogei.ambiente.gob.ar/files/inventario-nacional-gei-emisiones_hasta_2022.xlsx"

filename <- "inventario_nacional_gei.xlsx"

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{filename}"))

# # agrego la fuente
# agregar_fuente_raw(url = "https://inventariogei.ambiente.gob.ar/files/inventario-nacional-gei-emisiones_hasta_2018.xlsx", 
#                    institucion = "Ministerio Ambiente - Informe Bienal de Actualización", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emis_subsec_arg_2018.xlsx", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_subsector_arg_2018.R",
#                    nombre = "Inventario Nacional de Gases de Efecto Invernadero"
# )

actualizar_fuente_raw(id_fuente=131 ,
                      fecha_actualizar = "Sin informacion",
                      url = url,path_raw = filename, script = code_name,
                      nombre = "Inventario nacional de gases de efecto invernadero")

#list.files(tempdir())

#fuentes() %>% 
#  view()



