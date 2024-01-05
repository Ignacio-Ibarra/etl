# Preparacion de la estructura de carpetas
# Se lee la estructura de carpetas de datasets de subtopicos para replicarla

library(googledrive)
library(purrr)

# definir mail del usuario gmail con el que leer el drive
# 

googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))

df <- drive_ls(path = as_id("https://drive.google.com/drive/folders/16Out5kOds2kfsbudRvSoHGHsDfCml1p0"))

id_subtopicos <- df[df$name == "SUBTOPICOS",]$id

subtopicos <- drive_ls(id_subtopicos)

walk(subtopicos$name, \(x) {
  if (!dir.exists(glue::glue("scripts/{x}"))) {
    
    dir.create(glue::glue("scripts/{x}"))
  }
})

if (!dir.exists("data")) {dir.create("data")}

walk(subtopicos$name, \(x) {
  
  
  if (!dir.exists(glue::glue("data/{x}"))) {
    
    dir.create(glue::glue("data/{x}"))
    dir.create(glue::glue("data/{x}/datasets"))
    dir.create(glue::glue("data/{x}/datasets/outputs"))
    dir.create(glue::glue("data/{x}/datasets/raw"))
  }
})


