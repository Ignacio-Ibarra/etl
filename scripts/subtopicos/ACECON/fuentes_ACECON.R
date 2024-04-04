dir <- tempdir()

# cuentas nacionales - fundacion norte y sur
# pbi y pbi pc 
descargar_fuente("R36C9", dir =  dir)

# world economic outlook fmi
descargar_fuente("R34C2",dir =  dir)
descargar_fuente("R34C3", dir = dir)

# maddison database
descargar_fuente("R37C1", dir = dir)

# expectativa de vida 2018 - undp
descargar_fuente( "R41C0", dir = dir)

# escolaridad media 2018 - undp
descargar_fuente( "R40C0", dir = dir)

# Oferta y Demanda Globales trimestrales a precios 2004 - R38C6
descargar_fuente("R38C6", dir = dir)

# poblacion total pais - R39C8
descargar_fuente("R39C8", dir)
