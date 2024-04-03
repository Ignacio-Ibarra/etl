# Consolidacion de metadata
# Levanta las sheets de metadata y arma un csv con el conteo de subtopicos e items por url e institucion


# Lista los archivos dentro de la carpeta de Google Drive especificada por su ID y los guarda en un dataframe
df <- drive_ls(path = as_id(Sys.getenv("ARGENDATA_DRIVE")))

# Filtra el dataframe para obtener el ID de la carpeta que contiene los subtemas
id_subtopicos <- df[df$name == "SUBTOPICOS",]$id

# Lista los archivos o carpetas dentro de la carpeta de subtemas utilizando su ID
paths_subtopicos <- drive_ls(googledrive::as_id(id_subtopicos))

# Para cada ID en paths_subtopicos, lista los archivos dentro y los recopila en una lista
files_subtopicos <- map(paths_subtopicos$id, function(x) {
  drive_ls(googledrive::as_id(x))
})

# Combina los dataframes de archivos de subtemas en uno solo
files_subtopicos <- bind_rows(files_subtopicos)

# Filtra los archivos de metadatos que contienen "argendata -" en su nombre, excluyendo aquellos que contienen "ejemplo"
metadata_files <- files_subtopicos %>% 
  filter(str_detect(tolower(name), "argendata -")) %>% 
  filter(! str_detect(tolower(name), "ejemplo"))

# Para cada archivo de metadatos, lee su contenido saltando las primeras 6 filas y asumiendo que las columnas son tipo texto
metadata <- map2(metadata_files$id, metadata_files$name, function(x, y) {
  googlesheets4::read_sheet(x, skip = 6, col_types = "c") %>% 
    mutate(subtopico_nombre = gsub("ArgenData - ", "", y))
})

# Combina los datos leídos de todos los archivos de metadatos en un único dataframe
metadata <- bind_rows(metadata)

# Filtra el dataframe resultante para eliminar las filas donde todos los valores son NA (datos faltantes)
metadata <- metadata %>% 
  filter(!if_all(everything(), is.na))

# Escribe el dataframe final a un archivo CSV llamado "metadata.csv"
metadata %>% 
  write_csv("metadata.csv")

# Dataframe solo de fuentes

metadata %>% 
  distinct(fuente_nombre, url_path, institucion) %>% 
  write_csv("fuentes.csv")
