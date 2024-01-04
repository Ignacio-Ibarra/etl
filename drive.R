library(googledrive)
library(purrr)

drive_auth(email = "")

df <- drive_ls(path = as_id("https://drive.google.com/drive/folders/16Out5kOds2kfsbudRvSoHGHsDfCml1p0"))

id_subtopicos <- df[df$name == "SUBTOPICOS",]$id

subtopicos <- drive_ls(id_subtopicos)

map(subtopicos$name, \(x) {
  if (!dir.exists(glue::glue("scripts/{x}"))) {
    
    dir.create(glue::glue("scripts/{x}"))
  }
})

map(subtopicos$name, \(x) {
  if (!dir.exists(glue::glue("data/{x}"))) {
    
    dir.create(glue::glue("data/{x}"))
    dir.create(glue::glue("data/{x}/datasets"))
    dir.create(glue::glue("data/{x}/datasets/outputs"))
    dir.create(glue::glue("data/{x}/datasets/raw"))
  }
})


