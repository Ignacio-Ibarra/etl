library(tidyverse)
library(googledrive)

# reutuliza la auth drive cacheada para el mail guardado como var de environ de R
googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))

subtopico <- "ACECON"

source("scripts/subtopico_init.R")
