rm(list = ls())

df <- readr::read_csv(get_raw_path("R452C0"))

df <- df %>% janitor::clean_names()

# agrego fuente clean
agregar_fuente_clean(df = df, path_clean = "anomalia_temperatura_anual_paises.parquet",
                     id_fuente_raw = 452,
                     descripcion =  "Diferencia en °C de temperatura de superficie respecto al promedio de 1991 - 2020// Cita:Hersbach, H., Bell, B., Berrisford, P., Biavati, G., Horányi, A., Muñoz Sabater, J., Nicolas, J., Peubey, C., Radu, R., Rozum, I., Schepers, D., Simmons, A., Soci, C., Dee, D., Thépaut, J-N. (2023): ERA5 monthly averaged data on single levels from 1940 to present. Copernicus Climate Change Service (C3S) Climate Data Store (CDS), DOI: 10.24381/cds.f17050d7 (Accessed on 07-April-2025)",
                     nombre = "Diferencia en °C de temperatura de superficie respecto al promedio de 1991 - 2020 por país y año",
                     script = "limpieza_R452C0_1.R")

control <- comparar_fuente_clean(df, read_fuente_clean(295), id = 295, pk = c("code", "year"))

# actualizo fuente clean
actualizar_fuente_clean(df = df,comparacion = control,
                        id_fuente_clean = 295)
