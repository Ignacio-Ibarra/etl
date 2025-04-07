
#id_fuente <- 100
#fuente1 <- sprintf("R%sC0",id_fuente)

descargar_fuente_raw(id_fuente = 100, tempdir())

# me quedo con el sheet 2 y salteo 6 filas
co2_hist <- readxl::read_excel(get_temp_path("R100C0"),
                                sheet = 4,skip = 6)

# me quedo con las columnas año y co2 y multiplico año *-1
co2_hist <- co2_hist %>%
  rename(fecha_estimada = `EDC3_gas_a (yr)`, co2_ppm = `CO2 (ppmv)`) %>%
  mutate(fecha_estimada = fecha_estimada * -1)

# guardo csv
# write_csv_fundar(x = co2_hist,
#                  file = glue::glue("{tempdir()}/co2_hist.csv"))

# agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 100, 
#                      dir = tempdir(),
#                      path_clean = "co2_hist.csv",
#                      nombre = "Evolución CO2 histórico",
#                      script = "limpieza_CO2_historico_NOAA.R")

# actualizo fuente clean
actualizar_fuente_clean(
  id_fuente_clean = 26,
  df = co2_hist,
  comparacion = comparar_fuente_clean(co2_hist, id = 26, pk = "fecha_estimada")
)
