#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps() # para que guarde correctamente. 

url <- "https://dataverse.nl/api/access/datafile/354095"

# content <- httr2::req_perform(httr2::request(link))

# content$headers$`Content-disposition`

output_filename <- "penn_world_table1001.xlsx"

full_tmp_path <- glue::glue("{tempdir()}/{output_filename}")

download.file(url = url, 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = normalizePath(full_tmp_path))

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)

# agregar_fuente_raw(
#   url = link,
#   nombre = "Penn World Tables - 10.01",
#   institucion = "University of Groningen",
#   actualizable = T,
#   script = code_name,
#   path_raw = "penn_world_table1001.xlsx",
#   fecha_descarga = Sys.Date()
# )

actualizar_fuente_raw(
  id = "R92C0",
  url = url,
  actualizable = T,
  path_raw = output_filename,
  fecha_descarga = Sys.Date()
)
