#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-06-30")
fecha_actualizar <- "Sin informacion"

source("scripts/utils/indec_scraper_links.R")

id <- 114

pattern_cou <- ".*sh_cou_2019.*\\.xls"

result <- INDEC.cuentas_nacionales.extraer_links(id = id, pattern = pattern_cou)

url <- result$url

download_filename <- url %>% str_extract(., "sh.*\\.xls.*") # esta linea no debería ser así, debería proveer una string estatica

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = glue::glue("Cuentas Nacionales. Cuadros de oferta y utilización (COU). {result$text}")

# agregar_fuente_raw(url = url,
#                    institucion = "INDEC",
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 310,
                      url = url,
                      institucion = "Instituto Nacional de Estadísticas y Censos",
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)