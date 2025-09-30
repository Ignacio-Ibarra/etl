rm(list = ls())

df <- readr::read_csv(get_raw_path("R451C0"))

df <- df %>% janitor::clean_names()

# agrego fuente clean
agregar_fuente_clean(df = df, path_clean = "variacion_nivel_mar.parquet",
                     id_fuente_raw = 451,
                     descripcion =  "Variación absoluta del nivel del mar respecto al nivel promedio de 1993 - 2008// Cita: NOAA Climate.gov (2022) – processed by Our World in Data. “Global sea level as an average of Church and White (2011) and UHSLC data” [dataset]. NOAA Climate.gov, “Climate Change: Global Sea Level” [original data].",
                     nombre = "Variación absoluta por año del nivel del mar en mm respecto al nivel promedio de 1993 - 2008",
                     script = "limpieza_R451C0_1.R")

# actualizo fuente clean
actualizar_fuente_clean(df = df,
                        id_fuente_clean = 294)
