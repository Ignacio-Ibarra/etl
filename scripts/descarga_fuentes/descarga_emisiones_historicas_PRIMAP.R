
# emisiones_sector_global_1850-2014 co2  -----------

#   -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)

filename <- "primap_serie_emisiones_historicas.csv"
url <- "https://zenodo.org/records/17090760/files/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_22-Aug-2025.csv?download=1"
# traigo url del gráfico en owid al que corresponde el dataset 


# download.file(url = url, destfile = glue::glue("{tempdir()}/{filename}"), mode = "wb")

df <- read_csv(url)
# metadata <- yaml::read_yaml("https://zenodo.org/records/17090760/files/Guetschow_et_al_2025a-PRIMAP-hist_v2.7_final_no_rounding_22-Aug-2025.yaml?download=1")
# metadata <- metadata %>% yaml::as.yaml()

# data <- list(metadata = metadata, datos = df)

# guardo la data como csv en tmepdir
df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{filename}"))
# agrego la fuente
# agregar_fuente_raw(url = url,
#                    institucion =  "Gütschow, J., Busch, D., & Pflüger, M. (2025). The PRIMAP-hist national historical emissions time series (1750-2024) v2.7 (2.7) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17090760",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = filename,
#                    dir = tempdir(),
#                    script = code_name,
#                    nombre = "PRIMAP-hist serie de tiempo emisiones nacionales históricas"
# )

actualizar_fuente_raw(id_fuente=132 , 
                      institucion = "Gütschow, J., Busch, D., & Pflüger, M. (2025). The PRIMAP-hist national historical emissions time series (1750-2024) v2.7 (2.7) [Data set]. Zenodo. https://doi.org/10.5281/zenodo.17090760",
                      fecha_actualizar = "Sin informacion", url = url, nombre = "PRIMAP-hist serie de tiempo emisiones nacionales históricas",
                      path_raw = filename, script = code_name)
