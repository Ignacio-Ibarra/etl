# WDI World Bank (Banco Mundial) -----------------------------------------------------------------

# World Bank outlook database   ----------- 


# Cargo librería para interactuar con API

library(WDI)



WDIsearch('goods exports') %>% 
  as_tibble() # BX.GSR.MRCH.CD


WDIsearch('service exports') %>% 
  as_tibble() # BX.GSR.NFSV.CD



WDIsearch('exports of goods and services') %>% # Constant 2015
  as_tibble() %>% # NE.EXP.GNFS.KD
  print(n=Inf)

WDIsearch('exports of goods and services') %>% # % GDP
  as_tibble() %>% # NE.EXP.GNFS.ZS
  print(n=Inf)


WDIsearch('export unit value') %>% # Index 2015
  as_tibble() %>% # TX.UVI.MRCH.XD.WD
  print(n=Inf)


WDIsearch('export volume') %>% # Index 2000 = 100
  as_tibble() %>% # TX.QTY.MRCH.XD.WD
  print(n=Inf)

WDIsearch('trade') %>% # % GDP
  as_tibble() %>% #  NE.TRD.GNFS.ZS 
  print(n=Inf)


# Seleccion de variables de interes
vector <- c("BX.GSR.MRCH.CD", 
            "BX.GSR.NFSV.CD",
            "NE.EXP.GNFS.KD",
            "NE.EXP.GNFS.ZS", 
            "TX.UVI.MRCH.XD.WD",
            "TX.QTY.MRCH.XD.WD",
            "NE.TRD.GNFS.ZS "
            )


# Descarga de variables seleccionadas
data_wdi <-  WDI(indicator = vector) %>% 
  as_tibble()

# Guardo dataset en directorio temporal
data_wdi %>% 
  write_csv_fundar(file = glue::glue("{tempdir()}/wdi_bm.csv"))


# Agrego fuente a plantilla (primera vez)


# agregar_fuente_raw(
#   api = TRUE, # library(WDI)  https://github.com/vincentarelbundock/WDI
#   url = "https://databank.worldbank.org/source/world-development-indicators/preview/on",
#   nombre = "World Development Indicators",
#   institucion = "Banco Mundial",
#   actualizable = T,
#   fecha_descarga = Sys.Date(),
#   path_raw = "wdi_bm.csv",
#   script = "descarga_wdi_bm.R"
# )


# Actualizo fuente existete en base de datos
actualizar_fuente_raw(id_fuente = "R83C0",
                      url = "https://databank.worldbank.org/source/world-development-indicators/preview/on",
                      dir = tempdir())
