source("scripts/aux_functions.R")


df <- drive_ls(path = as_id("https://drive.google.com/drive/folders/16Out5kOds2kfsbudRvSoHGHsDfCml1p0"))

id_subtopicos <- df[df$name == "SUBTOPICOS",]$id

subtopicos <- drive_ls(id_subtopicos)


# eleccion del subtopico
# subtopico <- subtopicos$name[subtopicos$name == x]

# levanta los outputs del subtopico

outputs <- drive_ls(subtopicos$id[subtopicos$name == subtopico]) %>% 
  filter(name == "datasets") %>% 
  pull(id) %>% 
  drive_ls(.) %>% 
  filter(name == "outputs") %>% 
  pull(id) %>% 
  drive_ls(.) %>% 
  # seleccionar la entrega
  filter(str_detect(name, "primera_entrega")) %>% 
  pull(id) %>% 
  drive_ls(.)


# levanta lista de scripts del subtopico
# scripts_subtopico <- list.files(glue::glue("scripts/subtopicos/{subtopico}/"),
# full.names = T)


# le asigna nombres a los elementos de la lista
# names(scripts_subtopico) <-  list.files(glue::glue("scripts/subtopicos/{subtopico}/")) %>% 
# str_remove_all(pattern = "\\d_")

# scripts_subtopico <- as.list(scripts_subtopico)

# correr script de la lista para el subtopico
# source(scripts_subtopico$pib_pibpc_pob_arg_esp.R)

rm(list = c("df", "subtopicos", "id_subtopicos"))

