#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-01-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

source("scripts/utils/baci_data.R")

hs <- "HS96"
result <- BACI.get_download_links() %>% 
  dplyr::filter(grepl(hs, text))

institucion <- "Centre d'Études Prospectives et d'Informations Internationales (CEPII)"

nombre <- glue::glue("BACI: International Trade Database at the Product-Level. {result$text} - Version: {result$version}")

url <- result$link

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

options(timeout = max(Inf, getOption("timeout"))) #super necesario sino se cae la conexión.
download.file(url, destfile = destfile, mode = "wb")


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 422,
                      url = url,
                      institucion = institucion,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)