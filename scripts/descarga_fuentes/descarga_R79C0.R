
rm(list = ls())

#  Primary energy consumption per GDP -----------------------------------------------------------------------

url <- "https://ourworldindata.org/grapher/energy-intensity?country=CHE~DEU~IRN~ARG"


consumo_energia_pbipercapita <- owid_scraper(url)

consumo_energia_pbipercapita <- consumo_energia_pbipercapita %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anio",
               values_to = "valor")

consumo_energia_pbipercapita %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/consumo_energia_pbipercapita.csv")))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Primary energy consumption per GDP",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R79C0.R", # Nombre actualizado del script
#   path_raw = "consumo_energia_pbipercapita.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 79 con el ID correcto si es necesario):
actualizar_fuente_raw(79, script = "descarga_R79C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 