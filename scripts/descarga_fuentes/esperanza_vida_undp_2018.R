df <- jsonlite::fromJSON("https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=le")

df %>% 
  write_csv_fundar("data/_FUENTES/raw/esperanza_vida_2018_undp.csv")

agregar_fuente_raw(url = "https://api.hdrdata.org/CountryIndicators/filter?year=2018&indicator=le",
                   nombre = "Expectativa de Vida 2018 - Human Development Reports Data API", 
                   institucion = "Programa de las Naciones Unidas para el Desarrollo",
                   actualizable = F, path_raw = "esperanza_vida_2018_undp.csv",
                   script = "esperanza_vida_undp_2018.R", api = T
                  )

rm(list = ls())
