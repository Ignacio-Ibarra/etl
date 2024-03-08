# weo imf -----------------------------------------------------------------

# IMF outlook database   ----------- 
# descargo la base entera por mayor facilidad de referencia
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)



download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              mode = "wb",
              destfile = glue::glue("data/_FUENTES/raw/weo-imf.xls"))

# agregar_fuente_raw(
#   url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
#   nombre = "World Economic Outlook database",
#   institucion = "FMI",
#   actualizable = T,
#   fecha_descarga = Sys.Date(),
#   fecha_actualizar = "2024-11-01",
#   path_raw = "weo-imf.xls",
#   script = "descarga_weo_imf.R"
# )

actualizar_fuente(id_fuente = 1, fecha_descarga = Sys.Date())
