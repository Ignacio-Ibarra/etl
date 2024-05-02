link <- "https://dataverse.nl/api/access/datafile/421302"

content <- httr2::req_perform(httr2::request(link))

# content$headers$`Content-disposition`

download.file(url = link, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = normalizePath(glue::glue("{tempdir()}/maddisondatabase.xlsx")))

# agregar_fuente_raw(
#   url = link,
#   nombre = "Maddison Project Database",
#   institucion = "University of Groningen",
#   actualizable = T,
#   script = "descarga_maddison_db.R",
#   path_raw = "maddisondatabase.xlsx",
#   fecha_descarga = Sys.Date()
# )

actualizar_fuente_raw(
  id = 37,
  url = link,
  nombre = "Maddison Project Database",
  institucion = "University of Groningen",
  actualizable = T,
  script = "descarga_maddison_db.R",
  path_raw = "maddisondatabase.xlsx",
  fecha_descarga = Sys.Date()
)
