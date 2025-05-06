# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2025-01-01")
fecha_actualizar <- "Sin informacion"

source("scripts/utils/oecd_api.R")

agencyID = "OECD.ELS.SPD"

indicatorID = "DSD_SOCX_AGG@DF_SOCX_AGG"

start_year = 1980

end_year = 2023

data_selection_str = "all"

options(timeout = 30000)

url <- glue::glue("https://sdmx.oecd.org/public/rest/data/{agencyID},{indicatorID},1.0/{data_selection_str}?startPeriod={start_year}&endPeriod={end_year}&dimensionAtObservation=AllDimensions&format=csvfilewithlabels")


download_filename <- glue::glue("{agencyID}_{indicatorID}.csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

download.file(url, destfile)

nombre <- "Social Expenditure Database"

institucion = "OECD"

# agregar_fuente_raw(url = url,
#                    institucion = institucion,
#                    nombre = nombre,
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )



actualizar_fuente_raw(id_fuente = 403,
                      nombre = nombre, 
                      institucion = institucion, 
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename,
                      script = code_name)