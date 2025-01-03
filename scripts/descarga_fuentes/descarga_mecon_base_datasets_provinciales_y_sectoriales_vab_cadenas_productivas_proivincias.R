#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

fecha_actualizar <- "Sin informacion"

url <- "https://www.economia.gob.ar/catalogo-sspmi/nuevo_dataset/vab-por-cadenas-y-provincias-2018.csv"

nombre <- "Datasets Provinciales y Sectoriales. Cadenas Productivas. VAB por Cadenas y Provincias 2018"

institucion <- "Ministerio de Economía. Secretaría de Política Económica. Subsecretaría de Programación Regional y Sectorial"

download_filename <- "vab-por-cadenas-y-provincias-2018.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

# Desactivo la verificacion de SSL
GET(url, 
    config = config(ssl_verifypeer = FALSE), 
    write_disk(destfile, overwrite = TRUE))


# agregar_fuente_raw(url = url,
#                    nombre = nombre,
#                    institucion = institucion,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar)

actualizar_fuente_raw(id_fuente = 305,
                      url = url,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar, 
                      path_raw = download_filename,
                      actualizable = T,
                      script = code_name)