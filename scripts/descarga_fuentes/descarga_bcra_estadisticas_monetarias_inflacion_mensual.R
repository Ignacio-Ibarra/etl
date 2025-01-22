#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

source("scripts/utils/bcra_estadisticas_monetarias_api.R")

df_variables <- BCRA.estadisticas_monetarias.listar_estadisticas()$data

busqueda <- df_variables %>% dplyr::filter(idVariable == "27")

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date(busqueda$fecha)
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

id <- busqueda$idVariable

result <- BCRA.estadisticas_monetarias.get_data(idVariable = id)

url <- result$url

df_raw <- result$data

download_filename <- "bcra_inflacion_mensual.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% argendataR::write_csv_fundar(., destfile)

nombre = glue::glue("Estadísticas monetarias. Series y principales variables. {busqueda$descripcion}")

# agregar_fuente_raw(url = url,
#                    institucion = "Banco Central de la República Argentina",
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 313,
                      url = url,
                      nombre = nombre,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)