rm(list = ls())

# Electricity generation from nuclear ----------------------------------

url <- "https://ourworldindata.org/grapher/nuclear-energy-generation"



generacion_nuclear <- owid_scraper(url)

generacion_nuclear <- generacion_nuclear %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

generacion_nuclear %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/generacion_nuclear.csv")))

difs <- comparar_df(generacion_nuclear %>% 
              mutate(anio = as.numeric(anio)),
            read_csv(get_raw_path("R73C0")), 
            pk = c("name", "entities_name", "anio"))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Electricity generation from nuclear",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R73C0.R", # Nombre actualizado del script
#   path_raw = "generacion_nuclear.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 73 con el ID correcto si es necesario):
actualizar_fuente_raw(73, script = "descarga_R73C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 
