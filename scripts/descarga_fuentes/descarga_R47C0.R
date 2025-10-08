rm(list = ls())

# Global primary energy consumption by source ---------------
url <- "https://ourworldindata.org/grapher/global-energy-consumption-source?facet=none"

archivo <- "consumo_energia_global_owid.csv"

consumo_energia_global <- owid_scraper(url)

consumo_energia_global <- consumo_energia_global %>%
  select(-c(id, datasetId, entities_id)) %>%
  pivot_longer(cols = matches("\\d{4}"),
               names_to = "anio",
               values_to = "valor")

consumo_energia_global <- consumo_energia_global %>% 
  mutate(anio = as.numeric(anio))

consumo_energia_global %>% 
  write_csv_fundar(normalizePath(glue::glue("{tempdir()}/{archivo}")))

diferencia <- comparar_df(consumo_energia_global, df_anterior = read_csv(get_raw_path("R47C0")),
            pk = c("name", "anio", "title"))

# Descomentar y ejecutar la primera vez para registrar la fuente:
# agregar_fuente_raw(url = url,
#                    nombre = "Global primary energy consumption by source",
#                    institucion = "Our World in Data - OWID",
#                    script = "descarga_R47C0.R", # Nombre actualizado del script
#                    path_raw = "consumo_energia_global_owid.csv",
#                    directorio = NULL,
#                    api = F,
#                    fecha_actualizar = NULL, actualizable = F
#                    )

# Actualizar la fuente (reemplazar 47 con el ID correcto si es necesario):
actualizar_fuente_raw(47, script = "descarga_R47C0.R", directorio = tempdir(), fecha_actualizar = "Sin informacion") 
