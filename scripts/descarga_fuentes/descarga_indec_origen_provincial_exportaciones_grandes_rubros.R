code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(6)
fecha_ultima_actualizacion <- as.Date("2024-09-30")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


result <- INDEC.intercambio_comercial_argentino.extraer_links(id = 79, pattern = ".*sh_opex.*principales_grubros.*\\.xls.*")

url <- result$url

nombre <- result$text

download_filename <- nombre %>% janitor::make_clean_names(.) %>% paste0(.,".xls")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = destfile)


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = "INDEC",
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 274,
                      url = url,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)