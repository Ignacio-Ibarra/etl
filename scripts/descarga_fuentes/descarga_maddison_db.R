link <- "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx"

download.file(url = link, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/_FUENTES/raw/mpd2020.xlsx"))

agregar_fuente_raw(url = link, nombre = "Maddison Project Database 2020",
               institucion = "University of Groningen",actualizable = T,
               script = "descarga_maddison_db.R", path_raw = "mpd2020.xlsx",
               fecha_descarga = Sys.Date())

actualizar_fuente(id = 37, fecha_descarga = Sys.Date())