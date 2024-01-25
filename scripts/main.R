library(tidyverse)
library(googledrive)
# reutuliza la auth drive cacheada para el mail guardado como var de environ de R
googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))


source("scripts/aux_functions.R")



# asignar var subtopico con el nombre del subtopico a trabajar
subtopico <- "ACECON"

# asignar a outputs
outputs <- subtopico_init(subtopico_nombre = subtopico, entrega = "primera_entrega")

