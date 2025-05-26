#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "07_acuicultura_produccion_por_pais_ultimo_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R320C190' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_SPECIES_GROUPS
fuente3 <- 'R320C191' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS
fuente4 <- 'R320C192' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_WATERAREA_GROUPS



df_quantity <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_species <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_countries <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)  %>% 
  select(un_code, iso3 = iso3_code) %>% 
  dplyr::filter(iso3 !="", un_code != 532)


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long)

df_areas <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.)

un_codes <- df_countries %>% pull(un_code)

df_output <- df_quantity %>% 
  dplyr::filter(period == max(period)) %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(iso3, pais_nombre) %>% 
  summarise(produccion_acuicola_miles = sum(value, na.rm = T)/1000) %>% 
  ungroup()


df_output %>%
  argendataR::write_csv_fundar(output_name)



paises_seleccionados <- c("ARG", "BRA", "BOL", "CHL", "COL", "ECU", "PER", "PRY", "VEN")

plot_data <- df_output %>%
  filter(iso3 %in% paises_seleccionados) %>%
  arrange(produccion_acuicola_miles) %>%
  mutate(pais_nombre = factor(pais_nombre, levels = unique(pais_nombre)))


offset_texto <- 5

ggplot(plot_data, aes(x = produccion_acuicola_miles, y = pais_nombre, fill = case_when(
  pais_nombre == "Argentina" ~ "Argentina",
  TRUE ~ "Otros"
  ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +   
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Otros" = "#fc5a0a")) +
  geom_text(aes(
    label = format(round(produccion_acuicola_miles, 2), big.mark = ".", decimal.mark = ",", scientific = FALSE),
    x = produccion_acuicola_miles + offset_texto), 
    vjust = 0.5, hjust = 0) +  # Etiqueta con formato
  labs(x = "Miles de toneladas de peso vivo", y = "") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"), 
    axis.title = element_text(color = "black"),
    legend.position = "None"
  )
