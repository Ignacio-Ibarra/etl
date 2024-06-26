
# emisiones_sector_global_1850-2014 co2  -----------

# traigo url del gráfico en owid al que corresponde el dataset 
emisiones_glob_sector_1850_2014 <- readr::read_csv("https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv")

#ruta_archivo_temporal <- glue::glue("{tempdir()}/emisiones_glob_sect_1850_2014.csv")
#datos_desde_temporal <- read.csv(ruta_archivo_temporal)

#emisiones_glob_sect_1850_2014 <- readr::read_csv("https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv")
write.csv(emisiones_glob_sector_1850_2014, glue::glue("{tempdir()}/emisiones_glob_sector_1850_2014.csv"))

# agrego la fuente
agregar_fuente_raw(url = "https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv", 
                   institucion = "Postdam Institute for Climate Research", 
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = "emisiones_glob_sector_1850_2014.csv", 
                   dir = tempdir(),
                   script = "descarga_emisiones_global_sec_1850_2014.R",
                   nombre = "Emisiones globales co2 por sector año 1850 a 2014 (FUENTE)"
)

actualizar_fuente_raw(id_fuente=132 ,url = "https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv",
                      fecha_descarga = Sys.Date())

#list.files(tempdir())

#fuentes() %>% 
#  view()



