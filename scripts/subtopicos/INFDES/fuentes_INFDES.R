# EPH Total Urbano Microdatos 2016-2023
descargar_fuente("R49C16")

# EPH Total Urbano Diccionario Alomerados, Provincias, Regiones
descargar_fuente("R84C14")

# EPH descarga de todos los años
ids.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(codigo) %>% pull()
purrr::map(ids.eph, descargar_fuente)
