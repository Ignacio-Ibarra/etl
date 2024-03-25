df <- jsonlite::fromJSON("https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=mys")

df %>% 
  write_csv_fundar("data/_FUENTES/raw/media_anios_escolarizacion_undp_2018.csv")

agregar_fuente_raw(url = "https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=mys",
                   nombre = "Media de Años de Escolarizacion 2018 - Human Development Reports Data API", 
                   institucion = "Programa de las Naciones Unidas para el Desarrollo",
                   actualizable = F,
                   path_raw = "media_anios_escolarizacion_undp_2018.csv",
                   script = "media_anios_escolarizacion_undp_2018.R", api = T
)

rm(list = ls())
