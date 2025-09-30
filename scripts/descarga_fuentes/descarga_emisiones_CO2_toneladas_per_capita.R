rm(list = ls())

# emiciones co2 per capita en toneladas  -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emisiones_per_cap_co2_toneladas_url <- "https://ourworldindata.org/grapher/co-emissions-per-capita?facet=none"

archivo <- glue::glue("{tempdir()}/emisiones_per_cap_co2_toneladas.csv")

# asigno la data a un objeto
emisiones_per_cap_co2_toneladas<-owid_scraper(emisiones_per_cap_co2_toneladas_url)

# guardo la data como csv en tmepdir
emisiones_per_cap_co2_toneladas %>% 
  argendataR::write_csv_fundar(archivo)

# agrego la fuente
# agregar_fuente_raw(url = "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none", 
#                    institucion = "Our World in Data - OWID", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emisiones_per_cap_co2_toneladas.csv", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_CO2_toneladas_per_capita.R",
#                    nombre = "Emiciones co2 per capita en toneladas"
# )

# comparar_archivos(glue::glue("{tempdir()}/emisiones_per_cap_co2_toneladas.csv"), get_raw_path("R123C0"))
# diferencia <- comparar_df(df = read_csv(glue::glue("{tempdir()}/emisiones_per_cap_co2_toneladas.csv")),
#             df_anterior = read_csv(get_raw_path("R123C0")),
#             pk = c("id", "datasetId", "entities_id"))


actualizar_fuente_raw(id_fuente=123, 
                      fecha_actualizar = "Sin informacion")

#limpiar_temps()
#list.files(tempdir())

#fuentes() %>% 
#  view()

