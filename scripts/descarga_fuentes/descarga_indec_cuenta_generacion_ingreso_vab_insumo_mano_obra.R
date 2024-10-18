#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-10-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_cuentas_nacionales_scraper_links.R")


serie_cgi_id <- 49

pattern_vab <- ".*serie_cgi.*\\.xls"


url <- INDEC.cuentas_nacionales.extraer_links(id = serie_cgi_id, pattern = pattern_vab)


download_filename <- "serie_cgi.xls"

download.file(url,
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("{tempdir()}/{download_filename}"))


# agregar_fuente_raw(url = serie_cgi,institucion = "INDEC", actualizable = T,
#                fecha_descarga = Sys.Date(),
#                path_raw = "serie_cgi.xls",
#                script = "descarga_cuenta_generacion_ingreso_indec.R",
#                nombre = "Valor agregado bruto e insumo de mano de obra por sector de actividad económica"
#                 )

actualizar_fuente_raw(id_fuente = 35, 
                      path_raw = download_filename, 
                      url = url, 
                      fecha_actualizar = fecha_actualizar,
                      script = code_name)
