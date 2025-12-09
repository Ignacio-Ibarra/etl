# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

source("./scripts/utils/scraper_yvera.R")

result <- YVERA.cuenta_satelite_turismo_links(search_string = "CST-A 2019")

nombre_archivo <- glue::glue("Cuenta satélite de turismo de la Argentina. {result$text}")

institucion <- "Instituto Nacinal de Estadística y Censos (INDEC) - Ministerio de Turismo y Deportes (MINTURDEP)"

url <- result$url

download_filename <- basename(url)

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile, 
              method = "curl",
              extra = "-k")


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 493,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
