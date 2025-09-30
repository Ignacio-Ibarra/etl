rm(list = ls())

df <- readr::read_csv(get_raw_path("R121C0"))



# agrego fuente clean
agregar_fuente_clean(df = df, path_clean = "anomalias_temperatura_superficie_mar.parquet",
                     id_fuente_raw = 121,
                     descripcion =  "Evolución de las anomalías de temperatura en la superficie del mar. // Cita: Kennedy, J. J., Rayner, N. A., Atkinson, C. P., & Killick, R. E. (2019). An Ensemble Data Set of Sea Surface Temperature Change from 1850: The Met Office Hadley Centre HadSST.4.0.0.0 Data Set. Journal of Geophysical Research: Atmospheres, 124. https://doi.org/10.1029/2018JD029867. ",
                     nombre = "Evolución de la anomalía anual de temperatura de la superficie del mar relativo al promedio 1961 - 1990",
                     script = "limpieza_R121_C0_1.R")

# actualizo fuente clean
actualizar_fuente_clean(df = df, id_fuente_clean = 292)
