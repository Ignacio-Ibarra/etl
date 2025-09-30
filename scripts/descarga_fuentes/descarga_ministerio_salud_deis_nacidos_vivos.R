# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

source("scripts/utils/ministerio_salud_deis_scraper.R")

url <- "https://www.argentina.gob.ar/salud/deis/datos/nacidosvivos"

links_df <- DEIS.extraer_links(tema='nacidosvivos')

columnas_nacidosvivos <- c("PROVRES","TIPPARTO","SEXO","IMEDAD","ITIEMGEST","IMINSTRUC","IPESONAC","CUENTA")

rawlist <- DEIS.compilar(links_df, columnas_nacidosvivos) 

nombre <- glue::glue("Nacidos Vivos ({min(links_df$anio)}-{max(links_df$anio)})")

institucion <- "Ministerio de Salud. Dirección de Estadísticas e Información en Salud"

download_filename <- nombre %>% janitor::make_clean_names() %>% paste0(.,".json", collapse = "")

destfile <- glue::glue("{tempdir()}/{download_filename}")

rawlist %>% jsonlite::write_json(., destfile)


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 437,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
