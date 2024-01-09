library(tidyverse)
source("scripts/aux_functions.R")

# reutuliza la auth drive cacheada para el mail guardado como var de environ de R
googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))

# levanta diccionario de codigos iso y nombres de pais
iso_countrycodes <- read.csv("https://docs.google.com/spreadsheets/d/1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e/export?format=csv&id=1kK1Yu6gz5kEWe_i0vamiGttkXUH5H90e&gid=808722347")
iso_countrycodes <- iso_countrycodes %>% select(iso3, pais = iso3_desc_fundar)
# levanta lista de subtopicos de la propia estructura de directorios
subtopicos <- sort(list.dirs("scripts/subtopicos/", full.names = F)[list.dirs("scripts/subtopicos/", full.names = F) != ""])

# eleccion del subtopico
subtopico <- subtopicos[subtopicos == "ACECON"]

# levanta lista de scripts del subtopico
scripts_subtopico <- list.files(glue::glue("scripts/subtopicos/{subtopico}/"), full.names = T)

# le asigna nombres a los elementos de la lista
names(scripts_subtopico) <-  list.files(glue::glue("scripts/subtopicos/{subtopico}/")) %>% 
  str_remove_all(pattern = "\\d_")

scripts_subtopico <- as.list(scripts_subtopico)

# correr script de la lista para el subtopico
source(scripts_subtopico$pib_pibpc_pob_arg_esp.R)
