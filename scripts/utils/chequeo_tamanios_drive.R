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

datasets <- files_subtopicos %>% 
  filter(str_detect(tolower(name), "datasets")) 


datasets_dir <- map(datasets$id, .f = function(x) {
  googledrive::drive_ls(googledrive::as_id(x))
})

datasets_dir <- bind_rows(datasets_dir)

datasets_dir <- datasets_dir %>% 
  filter(name == "outputs")

outputs_dir <- map(datasets_dir$id, .f = function(x) {
  googledrive::drive_ls(googledrive::as_id(x))
})

outputs_dir <- bind_rows(outputs_dir)

outputs_dir <- outputs_dir %>% 
  filter(name == "datasets_primera_entrega")

datasets_primera_entrega_dir <- map(outputs_dir$id, .f = function(x) {
  googledrive::drive_ls(googledrive::as_id(x))
})

datasets_primera_entrega_dir <- bind_rows(datasets_primera_entrega_dir)

datasets_primera_entrega_dir <- datasets_primera_entrega_dir %>% 
  unnest_wider(col = drive_resource, names_sep = "_")

datasets_primera_entrega_dir <- datasets_primera_entrega_dir %>% 
  mutate(drive_resource_size = as.numeric(drive_resource_size)) 

datasets_primera_entrega_dir <- datasets_primera_entrega_dir %>% 
  select(name, id, drive_resource_size)


write_csv(datasets_primera_entrega_dir, file = "ouputs.csv")

datasets_primera_entrega_dir %>% 
  count(drive_resource_size > 900*1024)

datasets_primera_entrega_dir %>% 
  select(drive_resource_size) %>% 
  filter(drive_resource_size < 2*900*1024) %>% 
  ggplot() +
  geom_histogram(aes(x = drive_resource_size))

datasets_primera_entrega_dir %>% 
  filter(drive_resource_size > 900*1000) %>% 
  pull(name)
  view()
  count(as.numeric(drive_resource_size) > 900)
