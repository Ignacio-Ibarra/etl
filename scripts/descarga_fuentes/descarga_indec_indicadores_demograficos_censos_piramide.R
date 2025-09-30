# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)


fecha_actualizar <- "Sin informacion"

url <- "https://indecbeta.shinyapps.io/PAD_Demog_Arg/"

nombre <- "Indicadores Demográficos. Pirámide de población por sexo y edad. Años censales: 1869 a 2022"

institucion <- "Instituto Nacional de Estadística y Censos"

download_filename <- "INDEC_PAD_Seleccion-29_08_2025.xlsx" # copiado manualmente 

destfile <- glue::glue("{tempdir()}/{download_filename}")


# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 432,
                      url = url, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
