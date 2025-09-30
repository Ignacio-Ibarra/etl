rm(list = ls())


# emisiones_sector_global_2016 co2  -----------

# traigo url del gráfico en owid al que corresponde el dataset 

emisiones_sector_global_2016_url <- "https://ourworldindata.org/uploads/2020/09/Global-GHG-Emissions-by-sector-based-on-WRI-2020.xlsx"

archivo <- glue::glue("{tempdir()}/emisiones_sector_global_2016.xls")

download.file(url = emisiones_sector_global_2016_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = archivo)


# # agrego la fuente
# agregar_fuente_raw(url = "https://ourworldindata.org/uploads/2020/09/Global-GHG-Emissions-by-sector-based-on-WRI-2020.xlsx", 
#                    institucion = "Our World in Data - OWID", 
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emisiones_sector_global_2016.xls", 
#                    dir = tempdir(),
#                    script = "descarga_emisiones_co2_globales_por_sector_2016.R",
#                    nombre = "Emisiones globales co2 por sector año 2016"
# )

comparar_archivos(archivo, get_raw_path("R125C0"))

actualizar_fuente_raw(id_fuente=125 , 
                      fecha_actualizar = "Sin informacion")

#limpiar_temps()
#list.files(tempdir())

#fuentes() %>% 
#  view()



