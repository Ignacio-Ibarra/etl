# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2025-01-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/afip_operadores_comercio_exterior_informacion_agregada_scraper.R")

anio <- 2023
df_raw <- afip_comex_info_agregada.obtener_datos_expo(anio = anio)

url_general <- "https://www.afip.gob.ar/operadoresComercioExterior/informacionAgregada/informacion-agregada.asp"

nombre <- glue::glue("Operadores de comercio exterior. Información Agregada. Año {anio}")

institucion <- "Administración Federal de Ingresos Públicos"

download_filename <- glue::glue("total_expo_agregado_compilado_{anio}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = F,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 318,
                      url = url, 
                      nombre = nombre, 
                      institucion = "Instituto Nacional de Estadísticas y Censos",
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
