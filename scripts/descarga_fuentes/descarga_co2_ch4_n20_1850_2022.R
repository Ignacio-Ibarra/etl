# evolución historica CO2 NH4 N20 1850 - 2022  -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emi_anua_co2_ch4_n20_1850_2022_url <- "https://ourworldindata.org/grapher/ghg-emissions-by-gas?facet=none"

# asigno la data a un objeto
emi_anua_co2_ch4_n20_1850_2022<-owid_scraper(emi_anua_co2_ch4_n20_1850_2022_url)

# guardo la data como csv en tmepdir
emi_anua_co2_ch4_n20_1850_2022 %>% 
  argendataR::write_csv_fundar(glue::glue("{tempdir()}/emi_anua_co2_ch4_n20_1850_2022.csv"))
 
# agrego la fuente
agregar_fuente_raw(url = "https://ourworldindata.org/grapher/ghg-emissions-by-gas?facet=none", 
                   institucion = "Our World in Data - OWID", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emi_anua_co2_ch4_n20_1850_2022.csv", 
                   dir = tempdir(),
                   script = "descarga_co2_ch4_n20_1850_2022.R",
                   nombre = "Evolución emisiones anuales co2 ch4 n20"
)

actualizar_fuente_raw(id_fuente=114,url = "https://ourworldindata.org/grapher/ghg-emissions-by-gas?facet=none",
                      fecha_descarga = Sys.Date())

# limpiar_temps()
# list.files(tempdir())
# fuentes() %>% 
#   view()

