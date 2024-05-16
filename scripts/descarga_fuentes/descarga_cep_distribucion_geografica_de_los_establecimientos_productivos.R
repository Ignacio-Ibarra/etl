# CEP - Distribución geográfica de los establecimientos productivos


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# Descargo datos

# Desactivo la verificacion de SSL
options(download.file.method="curl", download.file.extra="-k -L")

url <- "https://cdn.produccion.gob.ar/cdn-cep/establecimientos-productivos/distribucion_establecimientos_productivos_sexo.csv"
destfile <- glue::glue("{tempdir()}/distribucion_establecimientos_productivos_sexo.csv")

download.file(url, destfile = destfile, mode = "wb")

agregar_fuente_raw(url = url,
                   nombre = "Distribución geográfica de establecimientos productivos por sexo",
                   institucion = "CEP",
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   directorio = tempdir(),
                   path_raw = "distribucion_establecimientos_productivos_sexo.csv",
                   script = code_name
)

# actualizar_fuente_raw(
#   id = "R99C0",
#   url = url,
#   actualizable = T,
#   path_raw = "cnphv2022_resultados_provisionales.xlsx",
#   fecha_descarga = Sys.Date()
# )