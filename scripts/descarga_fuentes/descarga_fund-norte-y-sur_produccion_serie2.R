# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vQ8xObYvj1XJIbw46N5haWHixuDBd2RLWmJSTHv9JEk4ptosKjUvuJ7PukNy9YE9w/pub?output=xls"

nombre_archivo <- "Producción. Serie 2"

institucion <- "Fundación Norte y Sur"

download_filename <- nombre_archivo %>% 
  janitor::make_clean_names() %>% 
  paste0(., ".xlsx")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile)

# 
# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre_archivo,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 485,
                      url = url, 
                      nombre = nombre_archivo, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
