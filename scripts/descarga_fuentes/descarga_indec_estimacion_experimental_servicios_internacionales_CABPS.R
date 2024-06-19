# Base de datos de la estimación experimental de servicios internacionales por la Clasificación Ampliada de Balance de Pagos de Servicios 2010 (CABPS) y país interlocutor para los años 2015-2022    -----------
comex_indec_url_servicios <- "https://www.indec.gob.ar/ftp/cuadros/economia/Base_servicios_internacionales_pais_CABPS.csv"


df_tibble <- data.table::fread(comex_indec_url_servicios, encoding = "Latin-1") %>% 
  janitor::clean_names() %>% 
  as_tibble()


argendataR::write_csv_fundar(x = df_tibble,
                             file = glue::glue('{tempdir()}/indec_estimacion_experimental_servicios_internacionales_CABPS.csv'))



#agregar_fuente_raw(url = comex_indec_url_servicios,
#                   institucion = "INDEC",
#                   actualizable = T,
#               fecha_descarga = Sys.Date(),
#               path_raw = "indec_estimacion_experimental_servicios_internacionales_CABPS.csv",
#               dir = glue::glue('{tempdir()}/'), 
#               script = "descarga_indec_estimacion_experimental_servicios_internacionales_CABPS.R",
#               nombre = "Ejercicio experimental de la estimación de servicios internacionales 
#               por la Clasificación Ampliada de Balance de Pagos de Servicios (CABPS) 2010 y país interlocutor"
#                )
#

argendataR::actualizar_fuente_raw(id_fuente = "R95C0" , fecha_descarga = Sys.Date())
