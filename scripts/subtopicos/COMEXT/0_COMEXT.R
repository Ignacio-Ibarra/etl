source("scripts/subtopicos/COMEXT/fuentes_COMEXT.R")

subtopico <-  "COMEXT"

analista <-  c("")

fuentes <- fuentes()

metadata_comext <- metadata(subtopico = "COMEXT")

fuentes_en_temp <- list.files(tempdir()) %>%  # Me guardo lista de fuentes descargadas en TEMP
  tibble::as_tibble()

