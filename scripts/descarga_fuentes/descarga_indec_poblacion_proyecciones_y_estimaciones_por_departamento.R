#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-18")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_scraper_links.R")

pattern_xls <- ".*\\.xls"

pob_depto_id = 119

resultado <- INDEC.poblacion_proyecciones.extraer_links(id = pob_depto_id, pattern = pattern_xls)

provincia <- resultado$provincia[1]

titulo <- resultado$titulo[1]

url <- resultado$link[1]

janitor_name <- paste0(titulo," ", provincia) %>% janitor::make_clean_names()

download_filename <- paste0(janitor_name,".xls") 

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

nombre <- glue::glue("Población. Proyecciones y Estimaciones. {titulo}. {provincia}")

agregar_fuente_raw(url = url,
                   institucion = "INDEC",
                   nombre = nombre,
                   actualizable = F,
                   path_raw = download_filename,
                   script = code_name,
                   fecha_actualizar = fecha_actualizar,
                   api = F
)

actualizar_fuente_raw(id_fuente = 223,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name) 