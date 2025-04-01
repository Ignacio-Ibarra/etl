# Wind energy capacity ----------------------------------------------------

url <- "https://ourworldindata.org/grapher/cumulative-installed-wind-energy-capacity-gigawatts"


capacidad_energia_eolica <- owid_scraper(url)

capacidad_energia_eolica <- capacidad_energia_eolica %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

capacidad_energia_eolica %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/capacidad_energia_eolica.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Wind energy capacity",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R75C0.R", # Nombre actualizado del script
#   path_raw = "capacidad_energia_eolica.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 75 con el ID correcto si es necesario):
actualizar_fuente_raw(75, script = "descarga_R75C0.R",
                      directorio = tempdir()) 