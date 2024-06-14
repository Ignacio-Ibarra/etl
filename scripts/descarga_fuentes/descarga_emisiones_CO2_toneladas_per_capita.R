
# emiciones co2 per capita en toneladas  -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emisiones_per_cap_co2_toneladas_url <- "https://ourworldindata.org/grapher/co-emissions-per-capita?facet=none"

# asigno la data a un objeto
emisiones_per_cap_co2_toneladas<-owid_scraper(emisiones_per_cap_co2_toneladas_url)

# guardo la data como csv en tmepdir
emisiones_per_cap_co2_toneladas %>% 
  argendataR::write_csv_fundar(glue::glue("{tempdir()}/emisiones_per_cap_co2_toneladas.csv"))

# agrego la fuente
agregar_fuente_raw(url = "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none", 
                   institucion = "Our World in Data - OWID", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emisiones_per_cap_co2_toneladas.csv", 
                   dir = tempdir(),
                   script = "descarga_emisiones_CO2_toneladas_per_capita.R",
                   nombre = "Emiciones co2 per capita en toneladas"
)

actualizar_fuente_raw(id_fuente=123,url = "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none",
                      fecha_descarga = Sys.Date())

#limpiar_temps()
#list.files(tempdir())

#fuentes() %>% 
#  view()

