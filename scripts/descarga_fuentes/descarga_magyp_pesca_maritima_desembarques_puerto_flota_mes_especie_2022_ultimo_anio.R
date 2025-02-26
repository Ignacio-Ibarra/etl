#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-02-28")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/magyp_scraper_links.R")

url_general <- "https://www.magyp.gob.ar/sitio/areas/pesca_maritima/desembarques/"

institucion <- "Secretaría de Agricultura, Ganadería y Pesca. Subsecretaria de Pesca y Acuicultura"

ultimo_anio <- year(Sys.Date()) - 1

nombre <- glue::glue("Desembarques por puerto, flota, especie y mes (2022-{ultimo_anio})")

download_filename <- glue::glue("desembarque_puerto_flota_especie_mes_2022_{ultimo_anio}.json")

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desde 2022 en adelante hay más de un tarquet
# Para data agregada elegir target == 'desembarque',
# Para data desagregada elegir target == "puerto_flota_especie_mes",
# Para data sobre Capacidad Máxima Permisible target == "cmp",
# En ese caso no es necesario pasar patron_archivo y el patron_pestania tampoco. 
lista_raw_2022_ultimo_anio <- MAGYP.obtener_datos(target = "puerto_flota_especie_mes", anios = 2022:ultimo_anio, patron_archivo = NULL, patron_pestania = NULL)


jsonlite::write_json(lista_raw_2022_ultimo_anio, path = destfile)


# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 331,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)