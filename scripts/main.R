library(tidyverse)
library(googledrive)
library(googlesheets4)
library(argendataR)

# reutuliza la auth drive cacheada para el mail guardado como var de environ de R
googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))
googlesheets4::gs4_auth(email = Sys.getenv("USER_GMAIL"))


# # fuentes <- read_csv("data/_FUENTES/fuentes_lista.csv")
# # 
# # # asignar var subtopico con el nombre del subtopico a trabajar
# subtopico <- "ACECON"
# # 
# # # asignar a outputs
# outputs <- subtopico_init(subtopico_nombre = subtopico, entrega = "primera_entrega")
