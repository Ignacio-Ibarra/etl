# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-03-31")
fecha_actualizar <- fecha_ultima_actualizacion  %m+% periodicidad

require(OECD)

# dataset_code <- "OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.0/VALU.....A"

# 
# # Desactivo la verificacion de SSL
# options(download.file.method="libcurl",
#         download.file.extra="-k -L --ssl-allow-unsafe-legacy-renegotiation"
# )

# df_raw <- OECD::get_dataset(dataset_code) # No funciona tira error de R base de la función download.file "HTTP status was 422 Unknown Error"

# df_raw <- read_delim("OECD.STI.PIE_DSD_TIVA_MAINLV@DF_MAINLV_1_0.csv", delim = ";")


download_filename <- "OECD.STI.PIE_DSD_TIVA_MAINLV@DF_MAINLV_1_0.csv"

destfile <- glue::glue("{tempdir()}/{download_filename}")

df_raw %>% write_csv(file = destfile)


url <- "https://sxs-boost-oecd.redpelicans.com/boost-disseminate/v2/sdmx/data/OECD.STI.PIE,DSD_TIVA_MAINLV@DF_MAINLV,1.0/VALU.....A?dimensionAtObservation=AllDimensions"

download.file(url, destfile)


# agregar_fuente_raw(url = url,
#                    institucion = "OECD",
#                    nombre = "Trade in Value Added (TiVA) 2023 edition. Value Added",
#                    actualizable = T,
#                    path_raw = download_filename,
#                    script = code_name,
#                    fecha_actualizar = fecha_actualizar,
#                    api = T
# )

as.character(fecha_actualizar)
actualizar_fuente_raw(id_fuente = 225,
                      fecha_actualizar = as.character(fecha_actualizar),
                      path_raw = download_filename)