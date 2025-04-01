# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


source("scripts/utils/scraper_unlp_fce_laboratorio_de_desarrollo_sectorial_y_territorial.R")

result <- UNLP_FCE_LAB.get_links_cadenas_productivas() %>% 
  dplyr::filter(text_links == "Cadenas agroalimentarias en Argentina: Año 2021 (Marzo 2023)")

nombre <- result$text_links

url <- result$links

institucion <- "Laboratorio de Desarrollo Sectorial y Territorial. Facultad de Ciencias Económicas. Universidad Nacional de La Plata"

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile = destfile, mode = "wb")

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 341,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)