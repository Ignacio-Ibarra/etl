#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-18")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_scraper_links.R")

agregados_macro_id <- 47

pattern_vbp_vab <- ".*sh_VBP_VAB_.*\\.xls"

result <- INDEC.cuentas_nacionales.extraer_links(id = agregados_macro_id, pattern = pattern_vbp_vab)

url <- result$url

download_filename <- url %>% str_extract(., "sh.*\\.xls.*") # esta linea no debería ser así, debería proveer una string estatica

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre = glue::glue("Cuentas Nacionales. Agregados Macroeconómicos (PIB). {result$text}")

# agregar_fuente_raw(url = url,
#                    institucion = "INDEC",
#                    nombre = "Cuentas Nacionales. Agregados Macroeconómicos (PIB). Series por sector de actividad económica: valor bruto de producción y valor agregado bruto, por trimestre",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 223,
                      url = url,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name) 