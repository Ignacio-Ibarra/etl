# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


source("scripts/utils/oecd_api.R")

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-03-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad


download_filename <- "OECD.STI.PIE_DSD_TIM_2023@DF_TIM_2023_1.0_EMPN.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")



# Define la URL de la API
url <- "https://sdmx.oecd.org/public/rest/data/OECD.STI.PIE,DSD_TIM_2023@DF_TIM_2023,1.0/EMPN...W..A?dimensionAtObservation=AllDimensions"


oecd_api.download_data_from_url(url, destfile)

# agregar_fuente_raw(url = url,
#                    institucion = "OECD",
#                    nombre = "Trade in Employment (TiM) 2023 edition. Employment",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 232,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)