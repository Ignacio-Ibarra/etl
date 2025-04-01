# Greenhouse gas ----------------------------------------------------------

url <- "https://ourworldindata.org/grapher/ghg-emissions-by-gas?facet=none"



emisiones_gases_invernadero <- owid_scraper(url)

emisiones_gases_invernadero <- emisiones_gases_invernadero %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anios", 
               values_to = "valor")

emisiones_gases_invernadero %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/emisiones_gases_invernadero.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Greenhouse gas emissions by gas, World, 1850 to 2022",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R82C0.R", # Nombre actualizado del script
#   path_raw = "emisiones_gases_invernadero.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

actualizar_fuente_raw(82, script = "descarga_R82C0.R",
                      directorio = tempdir()) 