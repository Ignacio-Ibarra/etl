# Biofuels production -----------------------------------------------------

url <- "https://ourworldindata.org/grapher/biofuel-production?country=~ARG"


produccion_biofuel <- owid_scraper(url)

produccion_biofuel <- produccion_biofuel %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

produccion_biofuel %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/produccion_biofuel.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Biofuels production",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R76C0.R", # Nombre actualizado del script
#   path_raw = "produccion_biofuel.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 76 con el ID correcto si es necesario):
actualizar_fuente_raw(76, script = "descarga_R76C0.R",
                      directorio = tempdir()) 