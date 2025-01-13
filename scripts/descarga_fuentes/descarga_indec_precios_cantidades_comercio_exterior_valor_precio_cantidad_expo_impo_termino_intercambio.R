#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

source("scripts/utils/indec_scraper_links.R")

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-11-05")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


result <- INDEC.intercambio_comercial_argentino.extraer_links(id = 41, pattern = ".*expindices.*" )

url <- result$url

title_raw <- glue::glue("Comercio exterior. Precios y cantidades del comercio exterior. Series históricas. {result$text}")


download_filename <- "indices_comex_indec.xls"

destfile <- glue::glue("{tempdir()}/{download_filename}")


download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = destfile)



actualizar_fuente_raw(id_fuente = 44,
                      url = url,
                      nombre = title_raw,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)
