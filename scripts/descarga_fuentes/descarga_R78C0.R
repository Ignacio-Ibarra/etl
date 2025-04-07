rm(list = ls())

#  Kaya identity: drivers of CO₂ emissions, World  -----------------------------------------------------------------------

url <- "https://ourworldindata.org/grapher/kaya-identity-co2"


identidad_kaya_factores <- owid_scraper(url)

identidad_kaya_factores <- identidad_kaya_factores %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anio",
               values_to = "valor")

identidad_kaya_factores %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/identidad_kaya_factores.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Kaya identity: drivers of CO₂ emissions",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R78C0.R", # Nombre actualizado del script
#   path_raw = "identidad_kaya_factores.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 78 con el ID correcto si es necesario):
actualizar_fuente_raw(78, script = "descarga_R78C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 