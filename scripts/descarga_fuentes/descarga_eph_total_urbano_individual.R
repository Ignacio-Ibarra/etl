#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

tipo_encuesta <- "individual"

anio_start <- 2016

#TODO el anio podría definirse programáticamente.
anio_end <- 2023  

version <- "v20240506"

ext <- "csv"

download_filename <- glue::glue("eph_total_urbano_{tipo_encuesta}_{anio_start}_{anio_end}_{version}.{ext}")

df <- eph::get_total_urbano(year = anio_start:anio_end, type = "individual")

df %>% write_csv_fundar(sprintf("data/_FUENTES/raw/%s",download_filename))

# agregar_fuente_raw(url = "https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos",
#                    nombre = glue::glue("Encuesta Permanente de Hogares Total Urbano, {str_to_title(tipo_encuesta)} ({anio_start} - {anio_end})"),
#                    institucion = "INDEC",
#                    actualizable = T,
#                    dir = "data/_FUENTES/raw/",
#                    path_raw = download_filename,
#                    script = code_name,
#                    api = T
# )

actualizar_fuente_raw(id_fuente = 49, actualizable = T, dir = "data/_FUENTES/raw/")