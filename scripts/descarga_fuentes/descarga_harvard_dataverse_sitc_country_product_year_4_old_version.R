#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"


download_filename <- "country_sitcproduct4digit_year.fst"

destfile <- glue::glue("{tempdir()}/{download_filename}")

url <- "https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/H8SFD2"

nombre = glue::glue("The Growth Lab at Harvard University. International Trade Data - SITC Rev. 1 - OLD VERSION")
institucion = "Harvard Dataverse"

# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)


actualizar_fuente_raw(id_fuente = 457,
                      nombre = nombre,
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)