# Electricity generation from hydropower ----------------------------------

url <- "https://ourworldindata.org/grapher/hydropower-consumption"


generacion_hidroelectrica <- owid_scraper(url)

generacion_hidroelectrica <- generacion_hidroelectrica %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

generacion_hidroelectrica %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/generacion_hidroelectrica.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Electricity generation from hydropower",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R72C0.R", # Nombre actualizado del script
#   path_raw = "generacion_hidroelectrica.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 72 con el ID correcto si es necesario):
actualizar_fuente_raw(72, script = "descarga_R72C0.R",
                      directorio = tempdir()) 