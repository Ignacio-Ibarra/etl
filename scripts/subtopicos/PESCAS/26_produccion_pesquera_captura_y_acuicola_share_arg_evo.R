#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "26_produccion_pesquera_captura_y_acuicola_share_arg_evo.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189'  # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R320C190'  # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_SPECIES_GROUPS
fuente3 <- 'R320C191'  # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS
fuente4 <- 'R320C192'  # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_WATERAREA_GROUPS
fuente5 <- 'R321C193'  # FAO Fisheries and Aquaculture Data Collection. Global Capture Production - File: Capture_Quantity.csv

# Cargar y procesar datos
df_captura <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet(.)

df_acuicola <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_species <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_countries <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.) %>% 
  select(un_code, iso3 = iso3_code) %>% 
  dplyr::filter(iso3 != "", un_code != 532)


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

df_areas <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.)

# Filtrar códigos UN
un_codes <- df_countries %>% pull(un_code)

# Procesar datos de acuicultura
df_prod_acuicola <- df_acuicola %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_acuicola = sum(value, na.rm = TRUE)) %>% 
  ungroup()

# Procesar datos de captura pesquera
df_prod_pesquera <- df_captura %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_captura = sum(value, na.rm = TRUE)) %>% 
  ungroup()

# Crear el dataframe de salida
df_output <- df_prod_acuicola %>% 
  full_join(df_prod_pesquera, join_by(anio, iso3, pais_nombre)) %>% 
  dplyr::filter(anio >= 1961) %>% 
  dplyr::filter(!(is.na(produccion_acuicola) & is.na(produccion_captura))) %>% 
  mutate(
    produccion_acuicola = replace_na(produccion_acuicola, 0),
    produccion_captura = replace_na(produccion_captura, 0),
    produccion_total = produccion_acuicola + produccion_captura
  ) %>% 
  group_by(anio) %>% 
  mutate(share_global = 100 * produccion_total / sum(produccion_total)) %>% 
  ungroup() %>% 
  dplyr::filter(iso3 == "ARG")


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )



ggplot(df_output, aes(x = anio, y = share_global)) + 
  geom_line(color = "#7ab5c5") + 
  labs(y = "% Producción global", x = "") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")
  )
