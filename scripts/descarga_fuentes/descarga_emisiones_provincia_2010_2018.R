

emisiones_arg_prov_arg_2010_2018_url <- "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx"

download.file(url = emisiones_arg_prov_arg_2010_2018_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/emis_arg_prov_2010_2018.xlsx"))

# agrego la fuente
agregar_fuente_raw(url = "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx", 
                   institucion = "Ministerio Ambiente - Informe Bienal de Actualización", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emis_arg_prov_2010_2018.xlsx", 
                   dir = tempdir(),
                   script = "descarga_emisiones_provincia_2010_2018.R",
                   nombre = "Emisiones por Provincia. Año 2010-2018"
)

actualizar_fuente_raw(id_fuente=157 ,url = "https://inventariogei.ambiente.gob.ar/files/desagregacion-provincial_hasta_2018.xlsx",
                      fecha_descarga = Sys.Date())

#list.files(tempdir())

#fuentes() %>% 
#  view()



