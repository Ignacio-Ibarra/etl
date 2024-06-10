# EPH Total Urbano Microdatos 2016-2023
descargar_fuente("R49C16")

# EPH Total Urbano Diccionario Alomerados, Provincias, Regiones
descargar_fuente("R84C14")

# EPH descarga de todos los años
ids.eph <- fuentes_raw() %>% filter(grepl("Encuesta Permanente de Hogares, Individual*", nombre)) %>% select(codigo) %>% pull()
purrr::map(ids.eph, descargar_fuente)

# Unemployment, total (% of total labor force) (modeled ILO estimate) 
descargar_fuente("R109C0")

# Unemployment, female (% of female labor force) (modeled ILO estimate)
descargar_fuente("R110C0")

# Unemployment, male (% of male labor force) (modeled ILO estimate)
descargar_fuente("R111C0")

# World Values Survey (2017 - 2022)
descargar_fuente("R105C0")

# Employment - Informality (productive definition) - SEDLAC (serie original y empalmada)
descargar_fuente("R115C31")

# Employment - Informality - SEDLAC (serie original y empalmada)
descargar_fuente("R115C32")

# Employment - Employment structure 3 - SEDLAC (serie original y empalmada)
descargar_fuente("R115C39")

# GDP per capita, PPP (constant 2021 international $)
descargar_fuente("R126C0")
