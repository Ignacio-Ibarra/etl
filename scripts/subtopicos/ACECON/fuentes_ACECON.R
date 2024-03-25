dir_clean <- "data/_FUENTES/clean/"
dir_raw <- "data/_FUENTES/raw/"
# cuentas nacionales - fundacion norte y sur
# pbi y pbi pc 
descargar_fuente_clean(path_clean = "pbi-pbipc-fyns.csv", dir =  dir_clean)

# world economic outlook fmi
descargar_fuente_clean(path_clean = "weo_imf.csv",dir =  dir_clean)
descargar_fuente_clean(path_clean = "diccionario_weo_imf.csv", dir = dir_clean)

# maddison database
descargar_fuente_clean(path_clean = "mpd2020.csv", dir = dir_clean)

# expectativa de vida 2018 - undp
descargar_fuente_raw(id_fuente = 41, dir = dir_raw)

# escolaridad media 2018 - undp
descargar_fuente_raw(id_fuente = 40, dir = dir_raw)
