# evolución historica GEI -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)



url <- "https://ourworldindata.org/grapher/population-long-run-with-projections.csv?v=1&csvType=full&useColumnShortNames=true"


filename <- "poblacion_regiones_owid.csv"
# metadata <- jsonlite::read_json(urlmetadata)
# 
# metadata <- metadata$columns[[1]]

df <-read_csv(url)

# data <- list(metadata = metadata, datos = df)

# guardo la data como csv en tmepdir
df %>% 
  write_csv_fundar(glue::glue("{tempdir()}/{filename}"))

# # agrego la fuente
# agregar_fuente_raw(url = url,
#                    institucion = "Our World in Data - OWID",
#                    actualizable = T,
#                    fecha_descarga = Sys.Date(),
#                    path_raw = filename,
#                    dir = tempdir(),
#                    script = code_name,
#                    fecha_actualizar = "Sin informacion",
#                    nombre = "De facto total population in a country, area or region as of 1 July of the year indicated."
# )

actualizar_fuente_raw(id_fuente= 472,
                      url = url,
                      nombre = "Population by country, available from 10,000 BCE to 2023, based on data and estimates from different sources.",
                      path_raw = filename,script = code_name,
                      fecha_actualizar = "Sin informacion")

# limpiar_temps()
# list.files(tempdir())
# fuentes() %>% 
#   view()

