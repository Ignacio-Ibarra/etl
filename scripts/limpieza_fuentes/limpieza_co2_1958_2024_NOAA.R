## descargo la fuente


## hay traer dos fuentes, una para mundo y otro para argentina
## la data hay que filtrarla para 2016 en ambas fuentes

## descargo fuente raw para mundo 
#descargar_fuente_raw(id_fuente = 161, tempdir())

#traigo la data
co2_1958_2024 <-data.table::fread(get_raw_path("R161C0"),skip = 34) %>% 
  janitor::clean_names()

  # elimino primeras filas  
co2_1958_2024 <- co2_1958_2024[-c(1, 2), ]

# me quedo con las variables anio mes y deseasonalized
co2_1958_2024_df <- co2_1958_2024 %>%
  select(1,2,5) %>%
  rename(anio = number, mes = v2, CO2_ppmv_deseasonalized = v5) %>% #cambio nombre columnas  
  mutate(across(everything(), as.numeric)) %>% 
  filter(mes == 3)# filtramos mes 3 para todos los anios porque donde arranca la serie es marzo 1958

# guardo csv
# write_csv_fundar(x = co2_1958_2024_df,
#                  file = glue::glue("{tempdir()}/co2_2958_2024_noaa.csv"))
# 
# # agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 161, 
#                      dir = tempdir(),
#                      path_clean = "co2_2958_2024_noaa.csv",
#                      nombre = "Concentración co2 1958-2024",
#                      script = "limpieza_co2_1958_2024_NOAA.R")

lista_comparacion <- comparar_fuente_clean(df = co2_1958_2024_df, id = 71, pk = c("anio", "mes"))

# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 71, comparacion = lista_comparacion, df = co2_1958_2024_df)



