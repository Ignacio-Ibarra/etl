
# evolución anual co2 region 2021 -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emisiones_anuales_co2_region_2021_url <- "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none"

# asigno la data a un objeto
emisiones_anuales_co2_region_2021<-owid_scraper(emisiones_anuales_co2_region_2021_url)

# guardo la data como csv en tmepdir
emisiones_anuales_co2_region_2021 %>% 
  argendataR::write_csv_fundar(glue::glue("{tempdir()}/emisiones_anuales_co2_region_2021.csv"))

# agrego la fuente
# agregar_fuente_raw(url = "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none", 
#                    institucion = "Our World in Data - OWID", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emisiones_anuales_co2_region_2021.csv", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_anuales_co2_region_2021.R",
#                    nombre = "Evolución emisiones anuales co2 región 2021"
# )

actualizar_fuente_raw(id_fuente=119,url = "https://ourworldindata.org/grapher/annual-co-emissions-by-region?facet=none",
                      fecha_actualizar = "Sin informacion")

#limpiar_temps()
#list.files(tempdir())

#fuentes() %>% 
#  view()

