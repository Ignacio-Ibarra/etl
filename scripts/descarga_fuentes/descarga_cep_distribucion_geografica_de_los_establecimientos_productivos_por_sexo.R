# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(1)
fecha_ultima_actualizacion <- as.Date("2023-12-31")
fecha_actualizar <- "Sin informacion"

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "https://cdn.produccion.gob.ar/cdn-cep/establecimientos-productivos/distribucion_establecimientos_productivos_sexo.csv"

download_filename <- "distribucion_establecimientos_productivos_sexo.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")


nombre = "Distribución geográfica de establecimientos productivos por sexo"

institucion = "Centro de Estudios para la Producción (CEP XXI). Secretaría de Coordinación de Producción. Ministerio de Economía"



# agregar_fuente_raw(url = url,
#                    nombre = "Distribución geográfica de establecimientos productivos por sexo",
#                    institucion = "CEP",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    directorio = tempdir(),
#                    path_raw = "distribucion_establecimientos_productivos_sexo.csv",
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 107,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename, 
                      script = code_name,
                      api = F)