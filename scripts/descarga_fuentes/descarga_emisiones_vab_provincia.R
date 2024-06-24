
emisiones_vab_provincias_url <- "https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content"

download.file(url = emisiones_vab_provincias_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/emisiones_vab_2018.xlsx"))

# agrego la fuente
agregar_fuente_raw(url = "https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content", 
                   institucion = "CEPAL - Desagregación provincial del valor agregado bruto de la Argentina, base 2004", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emisiones_vab_2018.xlsx", 
                   dir = tempdir(),
                   script = "descarga_emisiones_vab_provincia.R",
                   nombre = "Emisiones y valor agregado por provincia, 2018. (En MtCO2eq y en millones de pesos a precio de 2004)"
)

actualizar_fuente_raw(id_fuente=159 ,url = "https://repositorio.cepal.org/server/api/core/bitstreams/7399c6c9-0827-42da-b433-d176cb4107c7/content",
                      fecha_descarga = Sys.Date())

#list.files(tempdir())

#fuentes() %>% 
#  view()

