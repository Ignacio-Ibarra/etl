#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(12)
fecha_ultima_actualizacion <- as.Date("2024-02-28")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

tipo_encuesta <- "individual"

anio_start <- 2016

#TODO el anio podría definirse programáticamente.
anio_end <- 2023  

today <- format(Sys.Date(), format="%Y%m%d")

version <- paste0("v", today)

ext <- "csv"

download_filename <- glue::glue("eph_total_urbano_{tipo_encuesta}_{anio_start}_{anio_end}_{version}.{ext}")

destfile <- glue::glue("{tempdir()}/{download_filename}")

df <- eph::get_total_urbano(year = anio_start:anio_end, type = "individual")

df %>% write_csv_fundar(destfile)

# agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
#                    nombre = glue::glue("Encuesta Permanente de Hogares Total Urbano, {str_to_title(tipo_encuesta)} ({anio_start} - {anio_end})"),
#                    institucion = "INDEC",
#                    actualizable = T,
#                    dir = "data/_FUENTES/raw/",
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 49, 
                      path_raw = download_filename,
                      fecha_actualizar = fecha_actualizar)