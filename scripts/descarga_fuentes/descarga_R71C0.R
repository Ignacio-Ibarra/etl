rm(list = ls())


# Share of primary energy consumption that comes from low-carbon sources ------------

url <- "https://ourworldindata.org/grapher/low-carbon-share-energy?country=ARG~OWID_WRL~BRA~CHL~SWE"


archivo <- "participacion_fuentes_bajo_carbono_consumo.csv"

participacion_fuentes_bajo_carbono_consumo <- owid_scraper(url)

participacion_fuentes_bajo_carbono_consumo <- participacion_fuentes_bajo_carbono_consumo %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

participacion_fuentes_bajo_carbono_consumo %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/{archivo}")))

difs <- comparar_df(participacion_fuentes_bajo_carbono_consumo %>% 
              mutate(anio = as.numeric(anio)), read_csv(get_raw_path("R71C0")),
            pk = c("name", "entities_name", "anio"))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(
#   url = url,
#   nombre = "Share of primary energy consumption that comes from low-carbon sources",
#   institucion = "Our World in Data - OWID",
#   script = "descarga_R71C0.R", # Nombre actualizado del script
#   path_raw = "participacion_fuentes_bajo_carbono_consumo.csv",
#   directorio = NULL,
#   api = F,
#   fecha_actualizar = NULL,
#   actualizable = T
# )

# Actualizar la fuente (reemplazar 71 con el ID correcto si es necesario):
actualizar_fuente_raw(71, script = "descarga_R71C0.R",
                      directorio = tempdir(), fecha_actualizar = "Sin informacion") 
