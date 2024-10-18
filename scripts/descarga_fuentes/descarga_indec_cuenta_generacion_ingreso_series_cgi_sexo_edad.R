#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
limpiar_temps()

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-10-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/indec_cuentas_nacionales_scraper_links.R")

serie_cgi_id <- 49

pattern_vab <- ".*series_cgi_sexo_edad.*\\.xlsx"

url <- INDEC.cuentas_nacionales.extraer_links(id = serie_cgi_id, pattern = pattern_vab)

download_filename <- "series_cgi_sexo_edad.xlsx"

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    nombre = "Cuenta de generación del ingreso (CGI). Remuneración al trabajo asalariado, ingreso mixto e insumo de mano de obra, por sexo y tramos de edad",
#                    institucion = "INDEC",
#                    actualizable = T,
#                    fecha_actualizar = fecha_actualizar,
#                    path_raw = download_filename,
#                    script = code_name
# )

actualizar_fuente_raw(id_fuente = 228,
                     path_raw = download_filename, 
                     url = url, 
                     fecha_actualizar = fecha_actualizar,
                     script = code_name)
