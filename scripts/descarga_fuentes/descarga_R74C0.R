rm(list = ls())

# Solar energy capacity ----------------------------------

url <- "https://ourworldindata.org/grapher/installed-solar-PV-capacity"



capacidad_energia_solar <- owid_scraper(url)

capacidad_energia_solar <- capacidad_energia_solar %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

capacidad_energia_solar %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/capacidad_energia_solar.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Solar energy capacity",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R74C0.R", # Nombre actualizado del script
#   path_raw = "capacidad_energia_solar.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 74 con el ID correcto si es necesario):
actualizar_fuente_raw(74, script = "descarga_R74C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 