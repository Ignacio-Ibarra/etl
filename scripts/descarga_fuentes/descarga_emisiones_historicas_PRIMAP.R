
# emisiones_sector_global_1850-2014 co2  -----------

#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

filename <- "primap_serie_emisiones_historicas.json"
url <- "https://zenodo.org/records/17090760/files/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_no_rounding_22-Aug-2025.csv?download=1"
# traigo url del gráfico en owid al que corresponde el dataset 
df <- readr::read_csv(url)
metadata <- yaml::read_yaml("https://zenodo.org/records/17090760/files/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_no_rounding_22-Aug-2025.yaml?download=1")

data <- list(metadata = metadata, datos = df)

# guardo la data como csv en tmepdir
data %>% 
  jsonlite::write_json(glue::glue("{tempdir()}/{filename}"),
                       dataframe = "columns" )
# agrego la fuente
# agregar_fuente_raw(url = "https://datapub.gfz-potsdam.de/download/10.5880.PIK.2016.003/PRIMAP-hist_v1.0_14-Apr-2016.csv",
#                    institucion = "Postdam Institute for Climate Research",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = "emisiones_glob_sector_1850_2014.csv",
#                    dir = tempdir(),
#                    script = "descarga_emisiones_global_sec_1850_2014.R",
#                    nombre = "Emisiones globales co2 por sector año 1850 a 2014 (FUENTE)"
# )

actualizar_fuente_raw(id_fuente=132 , 
                      institucion = "Gütschow, J., Busch, D., & Pflüger, M. (2025). The PRIMAP-hist national historical emissions time series (1750-2024) v2.7 (2.7) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17090760",
                      fecha_actualizar = "Sin informacion", url = url, nombre = "PRIMAP-hist serie de tiempo emisiones nacionales históricas",
                      path_raw = filename, script = code_name)
