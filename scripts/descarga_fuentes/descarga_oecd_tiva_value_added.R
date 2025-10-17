# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


source("scripts/utils/oecd_api.R")

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

periodicidad <- months(3)
fecha_ultima_actualizacion <- as.Date("2025-01-01")
fecha_actualizar <- "Sin informacion"


# Define la URL de la API
url <- "https://sdmx.oecd.org/sti-public/rest/data/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.1/VALU.....A?dimensionAtObservation=AllDimensions&format=csvfilewithlabels"

df_raw <- oecd_api.download_data_from_url(url, delimitador = ",")

structure_name <- df_raw %>% distinct(STRUCTURE_NAME) %>% pull()
measure <- df_raw %>% distinct(Measure) %>% pull()

nombre <- glue::glue("{structure_name}. {measure}")

download_filename <- nombre %>% 
  janitor::make_clean_names() %>% 
  paste0(., ".csv")

destfile <- glue::glue("{tempdir()}/{download_filename}")

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



actualizar_fuente_raw(id_fuente = 225,
                      nombre = nombre, 
                      script = code_name,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)