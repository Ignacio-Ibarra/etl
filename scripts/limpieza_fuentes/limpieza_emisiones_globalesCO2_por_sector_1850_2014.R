

rm(list = ls())
"list.files(tempdir())
# "limpiar_temps()

## descargo fuente raw
descargar_fuente_raw(id_fuente = 132, tempdir())

# traigo la data y filtro co2
emis_global_sector_1850_2014 <- readr::read_csv(get_temp_path("R132C0")) %>% 
  filter(country == "EARTH" & category %in% c("CAT1", "CAT2", "CAT3", "CAT4", "CAT5", "CAT6", "CAT7") & unit == "GgCO2eq")

# dejo la variables que ncesitamos
emis_global_sector_1850_2014 <- emis_global_sector_1850_2014 %>% 
  select(-1,-2,-3,-5)

## transformo los datos
emis_global_sector_1850_2014_long <- pivot_longer(emis_global_sector_1850_2014, 
                                            cols = -c(unit,category),  # Columnas a mantener fijas
                                            names_to = "anio",             # Nombre para la columna de años
                                            values_to = "valor")          # Nombre para la columna de valores
# armo sectores
emis_global_sector_1850_2014_long_sectores <- emis_global_sector_1850_2014_long %>% 
mutate(sector = case_when(
  category %in% c("CAT1") ~ "Energía",
  category %in% c("CAT4", "CAT5") ~ "AGSyOUT",
  category %in% c("CAT2", "CAT3") ~ "PIUP",
  category == "CAT6" ~ "Residuos",
  category == "CAT7" ~ "Otros",
  TRUE ~ NA_character_  # Mantener NA para otras categorías no especificadas
)) %>% 
  group_by(anio, sector) %>%
  summarise(total_valor = sum(valor, na.rm = TRUE)) %>% 
  rename(valor_en_ggco2e=total_valor)

# guardo csv
write_csv_fundar(x = emis_global_sector_1850_2014_long_sectores,
                 file = glue::glue("{tempdir()}/emis_global_sectores_1850_2014.csv"))


# agrego fuente clean
# agregar_fuente_clean(id_fuente_raw = 132, 
#                      dir = tempdir(),
#                      # path_clean = "emis_global_sectores_1850_2014.csv",
#                      # nombre = "Emisiones globales por sector año 1850 - 2014",
#                      )

lista_comparacion <-  comparar_fuente_clean(df =  emis_global_sector_1850_2014_long_sectores %>% 
                                              mutate(anio = as.numeric(anio)),
                                            id = 56, 
                                            pk = c("anio", "sector"))


# actualizo fuente clean
actualizar_fuente_clean(id_fuente_clean = 56,
                        df = emis_global_sector_1850_2014_long_sectores,
                        comparacion = lista_comparacion)
