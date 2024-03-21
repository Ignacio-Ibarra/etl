dir_clean <- "data/_FUENTES/clean/"
# cuentas nacionales - fundacion norte y sur
# pbi y pbi pc 
descargar_fuente_clean(path_clean = "pbi-pbipc-fyns.csv", dir =  dir_clean)

# world economic outlook fmi
descargar_fuente_clean(path_clean = "weo_imf.csv",dir =  dir_clean)
descargar_fuente_clean(path_clean = "diccionario_weo_imf.csv", dir = dir_clean)

# maddison database
descargar_fuente_clean(path_clean = "mpd2020.csv", dir = dir_clean)

# pib - oferta y demanda a precios 2004 - indec
descargar_fuente_clean(path_clean = "oferta_demanda_pctes.csv", dir = dir_clean)

# poblacion argentina - indec
descargar_fuente_clean(path_clean = "poblacion_indec.csv", dir = dir_clean)

# serie cuenta generacion ingreso