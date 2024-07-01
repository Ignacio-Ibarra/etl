
# Aclaracion --------------------------------------------------------------


#' Este script requiere tener clonado el repo de transformers
#' el path de trasnformers deberia ser "~/transformers"
#' 

# flujo -------------------------------------------------------------------

directorios <- list.dirs("~/transformers")
directorios <- directorios[!grepl("\\.", directorios)]
directorios <-  directorios[!grepl("/transformers$", directorios)]
mappings <- list()


for (i in 1:length(directorios)) {
  archivos <- list.files(directorios[i], full.names = T)
  map_path <- grep("mappings.json", archivos, value = T)
  subtop <- str_extract(directorios[i], "(?<=transformers/).{6}")
  maps_subtop <- jsonlite::fromJSON(map_path)
  maps_subtop <- bind_rows(maps_subtop, .id = "dataset")
  mappings[[subtop]] <- maps_subtop
  
}

mappings <- mappings %>% bind_rows(.id = "subtopico") 

subtops_fuentes <- argendataR::metadata(subtopico = ".*")

fuentes_datasets <- subtops_fuentes %>% 
  distinct(subtopico_nombre, dataset_archivo, fuente_nombre, url_path, institucion)

fuentes_datasets <- fuentes_datasets %>% 
  mutate(subtopico_nombre =  gsub("Argendata - ", "",subtopico_nombre))

mappings <- mappings %>% 
  left_join(fuentes_datasets, by = c("dataset" = "dataset_archivo"))

writexl::write_xlsx(mappings, "data/fuentes_graficos.xlsx")
