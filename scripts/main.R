googledrive::drive_auth(email = Sys.getenv("USER_GMAIL"))

subtopicos <- sort(list.dirs("scripts/subtopicos/", full.names = F)[list.dirs("scripts/subtopicos/", full.names = F) != ""])

subtopico <- subtopicos[subtopicos == "ACECON"]
