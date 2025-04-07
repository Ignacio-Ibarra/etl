rm(list = ls())

# Electricity production by source -------------------------------------------------------------------------

url <- "https://ourworldindata.org/grapher/electricity-prod-source-stacked"


produccion_energia_por_fuente <- owid_scraper(url)

produccion_energia_por_fuente <- produccion_energia_por_fuente %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

produccion_energia_por_fuente %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/produccion_energia_por_fuente.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Electricity production by source",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R77C0.R", # Nombre actualizado del script
#   path_raw = "produccion_energia_por_fuente.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 77 con el ID correcto si es necesario):
actualizar_fuente_raw(77, script = "descarga_R77C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 