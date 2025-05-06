# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2025-01-01")
fecha_actualizar <- "Sin informacion"

source("scripts/utils/oecd_api.R")

url <- "https://sdmx.oecd.org/public/rest/data/OECD.SDD.TPS,DSD_ALFS@DF_SUMTAB,1.0/all?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

download_filename <- "OECD.SDD.TPS_DSD_ALFS@DF_SUMTAB.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw <- oecd_api.download_data_from_url(url, delimitador = ",")

nombre <- "Annual labour force survey, summary tables"

institucion = "OECD"

df_raw %>% argendataR::write_csv_fundar(., destfile)

# agregar_fuente_raw(url = url,
#                    institucion = "OECD",
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 401,
                      nombre = nombre, 
                      institucion = institucion, 
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name)