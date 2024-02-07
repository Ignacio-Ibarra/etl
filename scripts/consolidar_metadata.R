# Consolidacion de metadata
# Levanta las sheets de metadata y arma un csv con el conteo de subtopicos e items por url e institucion

library(googledrive)
library(tidyverse)

# definir mail del usuario gmail con el que leer el drive
drive_auth(email = Sys.getenv("USER_GMAIL"))

df <- drive_ls(path = as_id("https://drive.google.com/drive/folders/16Out5kOds2kfsbudRvSoHGHsDfCml1p0"))

id_subtopicos <- df[df$name == "SUBTOPICOS",]$id

paths_subtopicos <- drive_ls(id_subtopicos)

files_subtopicos <- map(paths_subtopicos$id, \(x) {
  drive_ls(x)
})

files_subtopicos <- bind_rows(files_subtopicos)

metadata_files <- files_subtopicos %>% 
  filter(str_detect(tolower(name),
                    "argendata -")) %>% 
  filter(! str_detect(tolower(name),
                    "ejemplo"))

googlesheets4::gs4_auth(email = Sys.getenv("USER_GMAIL"))

metadata <- map(metadata_files$id, \(x) {
  googlesheets4::read_sheet(x, skip = 6, col_types = "c")
})

metadata <- bind_rows(metadata)

metadata <- metadata %>% 
  filter(!if_all(everything(), is.na))


metadata %>% 
  write_csv("metadata.csv")
