
#fuente1 <- sprintf("R%sC0",id_fuente)

# traigo los datos del temporal

#datos <- file.path(tempdir(), "temp_aire_1850_2020.csv")

descargar_fuente_raw(id_fuente = 112, tempdir())
temp_aire_1850_2020 <- readr::read_csv(get_temp_path("R112C0"),
                               skip = 36)
# Cargo los datos 
#temp_aire_1850_2020 <- read.csv(datos,skip = 36) 
temp_aire_1850_2020 <- temp_aire_1850_2020[, 1:8]

# nombre de variables
temp_aire_1850_2020 <- rename(temp_aire_1850_2020,
                                  Year = `1`,
                                  gsta_brown_line_hist_ssp245 = `2`,
                                  gsta_brown_shading_bottom_hist_ssp245 = `3`,
                                  gsta_brown_shading_top_hist_ssp245 = `4`,
                                  gsta_green_line_hist_nat = `5`,
                                  gsta_green_shading_bottom_hist_nat = `6`,
                                  gsta_green_shading_top_hist_nat = `7`,
                                  gsta_black_obs = `8`)
# fecha
#temp_aire_1850_2020$fecha <- as.Date(paste(temp_aire_1850_2020$Year, "-01-01", sep = ""), format = "%Y-%m-%d")

# cambio nombre variable
temp_aire_1850_2020$anomalia_temperatura_deg_c <- temp_aire_1850_2020$gsta_black_obs
temp_aire_1850_2020$fecha <- as.Date(paste(temp_aire_1850_2020$Year, "-01-01", sep=""))

# me quedo con fecha y anomalia_temperatura_deg_c
temp_aire_1850_2020 <- temp_aire_1850_2020 %>%
  filter(!is.na(anomalia_temperatura_deg_c)) %>%
  select(10, 9)

# guardo csv
write_csv_fundar(x = temp_aire_1850_2020,
                 file = glue::glue("{tempdir()}/temp_aire_1850_2020.csv"))

# agrego fuente clean
agregar_fuente_clean(id_fuente_raw = 112, 
                     dir = tempdir(),
                     path_clean = "temp_aire_1850_2020.csv",
                     nombre = "Evolución temperatura aire 1850-2020",
                     script = "limpieza_temp_aire_1850_2020.R")

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean =28)


#dir_temporal <- tempdir()

# Listar los archivos en el directorio temporal
#archivos_temporales <- list.files(dir_temporal)

# Mostrar la lista de archivos temporales
#print(archivos_temporales)

