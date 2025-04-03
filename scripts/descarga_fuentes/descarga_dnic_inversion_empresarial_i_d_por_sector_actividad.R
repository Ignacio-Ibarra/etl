# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


source("scripts/utils/scraper_dnic.R")

result <- DNIC.listar_links_descarga(patron = ".*\\.csv$") %>% 
  dplyr::filter(grepl("Inversión empresarial en I\\+D según sector de actividad\\..*", filtered_texts))
  

nombre <- glue::glue("Sistema Integrado de Indicadores. {result$filtered_texts}")

url <- result$filtered_links

institucion <- "Dirección Nacional de Información Científica. Subsecretaría de Ciencias y Tecnología"

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

actualizar_fuente_raw(id_fuente = 342,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)