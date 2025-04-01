#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

source("scripts/utils/indec_scraper_links.R")

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-09-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


result <- INDEC.cuentas_nacionales.extraer_links(id = 47, pattern = ".*sh_oferta_demanda_\\d{2}_\\d{2}\\..*" )

url <- result$url

title_raw <- glue::glue("Cuentas Nacionales. Agregados macroeconómicos (PIB). {result$text}")


download_filename <- "sh_oferta_demanda.xls"

destfile <- glue::glue("{tempdir()}/{download_filename}")


download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = destfile)


# agregar_fuente_raw(url = oyd_cn_indec_url,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),path_raw = "sh_oferta_demanda.xls",
#                script = "descarga_oferta_demanda_indec.R",
#                nombre = "Series trimestrales de oferta y demanda globales"
#                 )

actualizar_fuente_raw(id_fuente = 38,
                      url = url,
                      nombre = title_raw,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)


