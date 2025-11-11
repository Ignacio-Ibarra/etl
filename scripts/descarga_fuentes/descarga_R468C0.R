# evolución historica GEI -----------
# limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_path <- this.path::this.path()
code_name <- code_path %>% str_split_1(., pattern = "/") %>% tail(., 1)




# traigo url del gráfico en owid al que corresponde el dataset 
url <- "https://ourworldindata.org/grapher/total-ghg-emissions.csv?v=1&csvType=full&useColumnShortNames=true"
urlmetadata <- "https://ourworldindata.org/grapher/total-ghg-emissions.metadata.json?v=1&csvType=full&useColumnShortNames=true"

filename <- "total_gei_emisiones_historico_paises.json"
metadata <- jsonlite::read_json(urlmetadata)

metadata <- metadata$columns[[1]]

df <-read_csv(url)

data <- list(metadata = metadata, datos = df)

# guardo la data como csv en tmepdir
data %>% 
  jsonlite::write_json(glue::glue("{tempdir()}/{filename}"),
                   # Date = "ISO8601",
                   # POSIXt =  "ISO8601", 
                   dataframe = "columns" )

# # agrego la fuente
agregar_fuente_raw(url = url,
                   institucion = "Our World in Data - OWID",
                   actualizable = T,
                   fecha_descarga = Sys.Date(),
                   path_raw = filename,
                   dir = tempdir(),
                   script = code_name,
                   fecha_actualizar = "Sin informacion",
                   nombre = "Emisiones anuales de gases de efecto invernadero, incluido el uso de la tierra"
)

# actualizar_fuente_raw(id_fuente= 468,
#                       url = url,
#                       path_raw = filename,
#                       fecha_actualizar = "Sin informacion")

# limpiar_temps()
# list.files(tempdir())
# fuentes() %>% 
#   view()

