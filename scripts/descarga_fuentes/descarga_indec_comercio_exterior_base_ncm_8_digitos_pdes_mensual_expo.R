# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-02-15")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


source("scripts/utils/indec_comex_ncm_8_digitos_impo_expo.R")

df_raw <- INDEC_COMEX.get_data(tipo_comercio = "exports", modalidad = "M", priorizar = "ncm")

url_general <- "https://comex.indec.gob.ar/#/database"

nombre <- glue::glue("Sistema de Consulta de Comercio Exterior de Bienes. Base de datos agregada. Exportaciones por producto (NCM, 8 dígitos) y país de destino. Mensual. Desde 2002")

institucion <- "Instituto Nacional de Estadísticas y Censos"

download_filename <- glue::glue("indec_comercio_exterior_base_ncm_8_digitos_pdes_mensual_expo.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url_general,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = F
# )

actualizar_fuente_raw(id_fuente = 319,
                      url = url_general, 
                      nombre = nombre, 
                      institucion = institucion,
                      fecha_actualizar = fecha_actualizar,
                      path_raw = download_filename,
                      script = code_name)
