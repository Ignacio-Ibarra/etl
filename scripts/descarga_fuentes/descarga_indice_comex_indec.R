# Índices de valor, precio y cantidad de las exportaciones, importaciones y términos del intercambio    -----------
comex_indec_url <- "https://www.indec.gob.ar/ftp/cuadros/economia/expindices_04.xls"


download.file(url = comex_indec_url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_FUENTES/raw/indices_comex_indec.xls"))

#agregar_fuente_raw(url = comex_indec_url, institucion = "INDEC", actualizable = T,
#               fecha_descarga = Sys.Date(),
#               path_raw = "indices_comex_indec.xls", dir = "data/_FUENTES/raw/", 
#               script = "descarga_indice_comex_indec.R",
#               nombre = "Índices de valor, precio y cantidad de las exportaciones, 
#               importaciones y términos del intercambio"
#                )

actualizar_fuente(id_fuente = 44 , fecha_descarga = Sys.Date())
