# Energy consumption by source, Argentina ----------------

url <- "https://ourworldindata.org/grapher/energy-consumption-by-source-and-country?country=~ARG"


consumo_energia_por_fuentes_arg <- owid_scraper(url)

consumo_energia_por_fuentes_arg <- consumo_energia_por_fuentes_arg %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

consumo_energia_por_fuentes_arg %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/consumo_energia_por_fuentes_arg_owid.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Energy consumption by source, Argentina",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R48C0.R", # Nombre actualizado del script
#   path_raw = "consumo_energia_por_fuentes_arg_owid.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 48 con el ID correcto si es necesario):
actualizar_fuente_raw(48, script = "descarga_R48C0.R",
                      directorio = tempdir()) 