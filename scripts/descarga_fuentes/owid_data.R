
owid_scraper <- function(url) {
  
  pedido <- httr2::request(url)
  
  rpta <- httr2::req_perform(pedido)
  
  contenido_html <- rpta %>% 
    httr2::resp_body_html()
  
  links <- contenido_html %>% 
    rvest::html_elements("link")
  
  links <- links[grepl("api.ourworldindata.org/v1/indicators",
                       rvest::html_attr(x = links, name = "href"))]
  
  links <- links %>% 
    rvest::html_attr("href")
  
  data_links <- links[grepl("\\.data", links)]
  
  metadata_links <- links[grepl("\\.metadata", links)]
  
  ids_data <- as.character(str_extract_all(data_links, "(?<=\\/)\\d+(?=\\.)", simplify = T))
  ids_metadata <- as.character(str_extract_all(data_links, "(?<=\\/)\\d+(?=\\.)", simplify = T))
  
  data_owid <- map(data_links, function(x) {
    jsonlite::fromJSON(x)
  })
  
  names(data_owid) <- ids_data
  
  data_owid <- bind_rows(data_owid, .id = "id")
  
  metadata_owid <- map(metadata_links, function(x) {
    jsonlite::fromJSON(x)
  })
  
  names(metadata_owid) <- ids_metadata 
  
  metadata_owid <- map(metadata_owid, function(x) {
    metadata_i <- x[c("id", "name", "unit", "shortUnit", "datasetId", "presentation","dimensions")]
    metadata_i$entities <- metadata_i$dimensions$entities$values
    metadata_i$title <- metadata_i$presentation$titlePublic
    metadata_i[c("id", "name", "unit", "shortUnit", "datasetId", "title","entities")]
  })
  
  metadata_owid <- lapply(metadata_owid, function(x) {purrr::compact(x)})
  
  metadata_owid <-  bind_rows(metadata_owid) %>%
    unnest_wider("entities", names_sep = "_", names_repair = "unique")
  
  metadata_owid <- metadata_owid %>% 
    mutate(id = as.character(id),
           entities_id = as.character(entities_id))
  
  data_owid_wide <- data_owid %>% 
    mutate(id = as.character(id),
           entities = as.character(entities)) %>% 
    pivot_wider(names_from = years, values_from = values, id_cols = c(id, entities)) 
  
  
  dataset_owid <- metadata_owid %>%
    left_join(data_owid_wide,  by = c("id" = "id",
                                      "entities_id" = "entities"))
  
  dataset_owid
}

# Global primary energy consumption by source ---------------
url <- "https://ourworldindata.org/grapher/global-energy-consumption-source?facet=none"

consumo_energia_global <- owid_scraper(url)

consumo_energia_global <- consumo_energia_global %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

consumo_energia_global %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/consumo_energia_global_owid.csv")))

agregar_fuente_raw(url = url,
                   nombre = "Global primary energy consumption by source",
                   institucion = "Our World in Data - OWID",
                   script = "owid_data.R",path_raw = "consumo_energia_global_owid.csv",
                   directorio = NULL,
                   api = F,
                   fecha_actualizar = NULL, actualizable = F
                   )

actualizar_fuente_raw(47)

# Energy consumption by source, Argentina ----------------

url <- "https://ourworldindata.org/grapher/energy-consumption-by-source-and-country?country=~ARG"


consumo_energia_por_fuentes_arg <- owid_scraper(url)

consumo_energia_por_fuentes_arg <- consumo_energia_por_fuentes_arg %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

consumo_energia_por_fuentes_arg %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/consumo_energia_por_fuentes_arg_owid.csv")))


# agregar_fuente_raw(
#   url = url,
#   nombre = "Energy consumption by source, Argentina",
#   institucion = "Our World in Data - OWID",
#   script = "owid_data.R",
#   path_raw = "consumo_energia_por_fuentes_arg_owid.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

actualizar_fuente_raw(48,
                      directorio = tempdir())

# Share of primary energy consumption that comes from low-carbon sources ------------

url <- "https://ourworldindata.org/grapher/low-carbon-share-energy?country=ARG~OWID_WRL~BRA~CHL~SWE"


participacion_fuentes_bajo_carbono_consumo <- owid_scraper(url)

participacion_fuentes_bajo_carbono_consumo <- participacion_fuentes_bajo_carbono_consumo %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

participacion_fuentes_bajo_carbono_consumo %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/participacion_fuentes_bajo_carbono_consumo.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Share of primary energy consumption that comes from low-carbon sources",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "participacion_fuentes_bajo_carbono_consumo.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(71,
                      directorio = tempdir())



# Electricity generation from hydropower ----------------------------------

url <- "https://ourworldindata.org/grapher/hydropower-consumption"

generacion_hidroelectrica <- owid_scraper(url)

generacion_hidroelectrica <- generacion_hidroelectrica %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

generacion_hidroelectrica %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/generacion_hidroelectrica.csv")))


# agregar_fuente_raw(
#   url = url,
#   nombre = "Electricity generation from hydropower",
#   institucion = "Our World in Data - OWID",
#   script = "owid_data.R",
#   path_raw = "generacion_hidroelectrica.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

actualizar_fuente_raw(72,
                      directorio = tempdir())

# Electricity generation from nuclear ----------------------------------


url <- "https://ourworldindata.org/grapher/nuclear-energy-generation"

generacion_nuclear <- owid_scraper(url)

generacion_nuclear <- generacion_nuclear %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

generacion_nuclear %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/generacion_nuclear.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Electricity generation from nuclear",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "generacion_nuclear.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(73,
                      directorio = tempdir())

# Solar energy capacity ----------------------------------



url <- "https://ourworldindata.org/grapher/installed-solar-PV-capacity"

capacidad_energia_solar <- owid_scraper(url)

capacidad_energia_solar <- capacidad_energia_solar %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

capacidad_energia_solar %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/capacidad_energia_solar.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Solar energy capacity",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "capacidad_energia_solar.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(74,
                      directorio = tempdir())


# Wind energy capacity ----------------------------------------------------


url <- "https://ourworldindata.org/grapher/cumulative-installed-wind-energy-capacity-gigawatts"

capacidad_energia_eolica <- owid_scraper(url)

capacidad_energia_eolica <- capacidad_energia_eolica %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

capacidad_energia_eolica %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/capacidad_energia_eolica.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Wind energy capacity",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "capacidad_energia_eolica.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(75,
                      directorio = tempdir())



# Biofuels production -----------------------------------------------------



url <- "https://ourworldindata.org/grapher/biofuel-production?country=~ARG"

produccion_biofuel <- owid_scraper(url)

produccion_biofuel <- produccion_biofuel %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

produccion_biofuel %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/produccion_biofuel.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Biofuels production",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "produccion_biofuel.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(76,
                      directorio = tempdir())


# Electricity production by source -------------------------------------------------------------------------




url <- "https://ourworldindata.org/grapher/electricity-prod-source-stacked"

produccion_energia_por_fuente <- owid_scraper(url)

produccion_energia_por_fuente <- produccion_energia_por_fuente %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anios",
               values_to = "valor")

produccion_energia_por_fuente %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/produccion_energia_por_fuente.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Electricity production by source",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "produccion_energia_por_fuente.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(77,
                      directorio = tempdir())



#  Kaya identity: drivers of CO₂ emissions, World  -----------------------------------------------------------------------

url <- "https://ourworldindata.org/grapher/kaya-identity-co2"


identidad_kaya_factores <- owid_scraper(url)

identidad_kaya_factores <- identidad_kaya_factores %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anios",
               values_to = "valor")

identidad_kaya_factores %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/identidad_kaya_factores.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Kaya identity: drivers of CO₂ emissions",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "identidad_kaya_factores.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(78,
                      directorio = tempdir())


#  Primary energy consumption per GDP -----------------------------------------------------------------------



url <- "https://ourworldindata.org/grapher/energy-intensity?country=CHE~DEU~IRN~ARG"


consumo_energia_pbipercapita <- owid_scraper(url)

consumo_energia_pbipercapita <- consumo_energia_pbipercapita %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anios",
               values_to = "valor")

consumo_energia_pbipercapita %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/consumo_energia_pbipercapita.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Primary energy consumption per GDP",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "consumo_energia_pbipercapita.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(79,
                      directorio = tempdir())


# Carbon intensity of electricity generation ------------------------------

url <- "https://ourworldindata.org/grapher/carbon-intensity-electricity"


intensidad_carbon <- owid_scraper(url)

intensidad_carbon <- intensidad_carbon %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d+"),
               names_to = "anios",
               values_to = "valor")

intensidad_carbon %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/intensidad_carbon.csv")))


agregar_fuente_raw(
  url = url,
  nombre = "Carbon intensity of electricity generation",
  institucion = "Our World in Data - OWID",
  script = "owid_data.R",
  path_raw = "intensidad_carbon.csv",
  directorio = NULL,
  api = F,
  fecha_actualizar = NULL,
  actualizable = T
)

actualizar_fuente_raw(80,
                      directorio = tempdir())
