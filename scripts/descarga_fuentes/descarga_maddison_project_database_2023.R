#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

periodicidad <- months(24)
fecha_ultima_actualizacion <- as.Date("2024-04-30")
fecha_actualizar <- fecha_ultima_actualizacion + periodicidad

link <- "https://dataverse.nl/api/access/datafile/421302"

content <- httr2::req_perform(httr2::request(link))

# content$headers$`Content-disposition`

download.file(url = link, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = normalizePath(glue::glue("{tempdir()}/maddisondatabase_2023.xlsx")))

# agregar_fuente_raw(
#   url = link,
#   nombre = "Maddison Project Database (2023)",
#   institucion = "University of Groningen",
#   actualizable = T,
#   script = code_name,
#   fecha_actualizar = as.character(fecha_actualizar),
#   path_raw = "maddisondatabase_2023.xlsx",
#   fecha_descarga = Sys.Date()
# )

actualizar_fuente_raw(
  id_fuente = 219,
  url = link,
  path_raw = "maddisondatabase_2023.xlsx",
  fecha_descarga = Sys.Date()
)