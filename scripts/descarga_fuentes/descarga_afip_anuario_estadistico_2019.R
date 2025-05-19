#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-12-20")
fecha_actualizar <- "Sin informacion"


source("scripts/utils/afip_anuario_estadistico_scraper.R", encoding = "UTF-8")


year = 2019

# Extraigo links y almaceno datos en df
resultado <- afip_anuario_estadistico.extraer_links_afip() %>% 
  dplyr::filter(anio == year)

url <- resultado$url

nombre <- glue::glue("Anuario estadísticas tributarias - {year}")

institucion <- "Administracion Federal de Ingresos Publicos"

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

actualizar_fuente_raw(id_fuente = 253,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
