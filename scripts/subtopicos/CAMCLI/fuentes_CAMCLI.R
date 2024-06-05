################################################################################
##                              Fuentes del Subtopico                         ##
################################################################################


#-- Fuente 1: EMISIONES GASES INV / Codigo de fuente: R82C0
descargar_fuente_clean(26, dir = tempdir())
descargar_fuente_clean(82, dir = tempdir())

descargar_fuente(codigo = "R82C0")
descargar_fuente(codigo = "R100C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R100C26") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R112C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R112C28") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R114C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R118C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R119C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R121C0") # acá va temp aire 1850-2020
descargar_fuente(codigo = "R122C0") # acá va temp aire 1850-2020


fuentes_raw() %>% 
  view()
# source("scripts/subtopicos/CAMCLI/4_evolucion_temperatura_aire_1850_2020.R)"
