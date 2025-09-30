rm(list = ls())

df <- data.table::fread(get_raw_path("R122C0"),skip = 35)

columnas <- c(
  "Year",
  "Month",
  "Monthly_Anomaly",
  "Monthly__Unc.",
  "Annual_Anomaly",
  "Annual_Unc.",
  "Five_Year_Anomaly",
  "Five_Year_Unc.",
  "Ten_Year_Anomaly",
  "Ten_Year_Unc.",
  "Twenty_Year_Anomaly",
  "Twenty_Year_Unc"
)

colnames(df) <- columnas


df <- df %>% janitor::clean_names()

df <- df %>% as_tibble()

# agrego fuente clean# agrego fuen... = te clean
agregar_fuente_clean(df = df, path_clean = "anomalias_temperatura_superficie_tierra.parquet",
                     id_fuente_raw = 122,
                     descripcion =  "Evolución de las anomalías de temperatura en la superficie de la tierra",
                     nombre = "Evolución de la anomalía mensual de temperatura de la superficie de la tierra relativo al promedio Ene 1951- Dic 1980 ",
                     script = "limpieza_R122C0_1.R")

# actualizo fuente clean
actualizar_fuente_clean(df = df, id_fuente_clean = 293)
