# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


source("scripts/utils/oecd_api.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-03-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


download_filename <- "OECD.STI.PIE_DSD_TIVA_MAINLV@DF_MAINLV_1_0.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")



# Define la URL de la API
url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.0/VALU.....A?dimensionAtObservation=AllDimensions"


oecd_api.download_data_from_url(url, destfile)

# agregar_fuente_raw(url = url,
#                    institucion = "OECD",
#                    nombre = "Trade in Value Added (TiVA) 2023 edition. Value Added",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 225,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)