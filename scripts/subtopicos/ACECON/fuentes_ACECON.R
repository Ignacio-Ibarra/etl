dir_clean <- "data/_FUENTES/clean/"
dir_raw <- "data/_FUENTES/raw/"
# cuentas nacionales - fundacion norte y sur
# pbi y pbi pc 
descargar_fuente_clean(id_fuente = "R36C9", dir =  dir_clean)

# world economic outlook fmi
descargar_fuente("R34C2",dir =  dir_clean)
descargar_fuente("R34C3", dir = dir_clean)

# maddison database
descargar_fuente("R37C1", dir = dir_clean)

# expectativa de vida 2018 - undp
descargar_fuente(id_fuente = "R41C0", dir = dir_raw)

# escolaridad media 2018 - undp
descargar_fuente(id_fuente = "R40C0", dir = dir_raw)
