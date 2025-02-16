#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-12-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/fao_fishstats.R")

# Esto puede requerir que se modifique manualmente por ahora. 
# Se obtiene ingresando a la pagina https://www.fao.org/fishery/en/collection/aquaculture
# Insepccionar página, ver request https://www.fao.org/fishery/services/collection/fishery/collection/search/alias?alias=aquaculture&lang=en
# obtener el token del header, copiar y pegar la string en la variable token_manual.
token_manual <- "O72UKMYbVIXpclxailuagjgH-NplKbkUZhqfYXqmSk0MQJNh0yAKmkTG0G3uYXxXgbjd2s_RvIJpwhTXzmiDQROuMh8g9L6bdyGw52VOAPfbM6-7oklESd1JE_3XjK3x"

result <- FAO_FISHSTATS.get_download_link(topic = "capture", token_manual = token_manual)

url <- result$link

institucion <- result$institution

nombre <- glue::glue("{result$serie}. {result$title}")

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 321,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)