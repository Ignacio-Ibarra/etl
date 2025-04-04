rm(list = ls())

# Carbon intensity of electricity generation ------------------------------

url <- "https://ourworldindata.org/grapher/carbon-intensity-electricity"


intensidad_carbon <- owid_scraper(url)

intensidad_carbon <- intensidad_carbon %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anio",
               values_to = "valor")

intensidad_carbon %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/intensidad_carbon.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Carbon intensity of electricity generation",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R80C0.R", # Nombre actualizado del script
#   path_raw = "intensidad_carbon.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 80 con el ID correcto si es necesario):
actualizar_fuente_raw(80, script = "descarga_R80C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 