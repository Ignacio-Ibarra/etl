# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CAMCLI"
output_name <- "anomalia_temperatura_anual_argentina"
branch <- "main"
analista <- ""

fuente1 <- 'R452C295'

df <- read_fuente_clean(295)


df_output <- df %>% 
  filter(code == "ARG") %>% 
  mutate(temperature_anomaly = temperature_anomaly-mean(df$temperature_anomaly[df$year %in% 1940:1969])) %>% 
  rename(geonombreFundar = entity, geocodigoFundar = code )

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>%
  rename(geocodigoFundar = codigo_pais, geonombreFundar = pais)


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar")
)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = list("Dataset nuevo"), 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = c("geocodigoFundar", "year"),
    descripcion_columnas = list(
      "geonombreFundar" = "Nombre de entidad geografica",
      "geocodigoFundar"   = "Codigo en geonomenclador argendata" ,
      "year"    = "Año",
      "temperature_anomaly" = "Diferencia de temperatura en °C relativa al promedio 1940-1969"
      
    ),
    unidades = list("temperature_anomaly" = "°C")
  )


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = branch)
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = branch)
