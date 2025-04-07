


emisiones_subsec_arg_2018_url <- "https://inventariogei.ambiente.gob.ar/files/inventario-nacional-gei-emisiones_hasta_2018.xlsx"

download.file(url = emisiones_subsec_arg_2018_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/emis_subsec_arg_2018.xlsx"))

# # agrego la fuente
# agregar_fuente_raw(url = "https://inventariogei.ambiente.gob.ar/files/inventario-nacional-gei-emisiones_hasta_2018.xlsx", 
#                    institucion = "Ministerio Ambiente - Informe Bienal de Actualización", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emis_subsec_arg_2018.xlsx", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_subsector_arg_2018.R",
#                    nombre = "Emisiones subsector Argentina año 2018"
# )

actualizar_fuente_raw(id_fuente=131 ,
                      fecha_actualizar = "Sin informacion")

#list.files(tempdir())

#fuentes() %>% 
#  view()



