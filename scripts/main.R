library(tidyverse)
library(googledrive)
library(googlesheets4)
library(argendataR)
library(data.table)
library(rvest)
library(httr)

# library(WDI)

# reutuliza la auth drive cacheada para el mail guardado como var de environ de R
googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))
googlesheets4::gs4_auth(email = Sys.getenv("USER_GMAIL"))

# source("scripts/utils/crear_directorios.R")
