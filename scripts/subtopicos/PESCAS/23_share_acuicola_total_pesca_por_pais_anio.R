#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "23_share_acuicola_total_pesca_por_pais_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R320C190' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_SPECIES_GROUPS
fuente3 <- 'R320C191' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_COUNTRY_GROUPS
fuente4 <- 'R320C192' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: CL_FI_WATERAREA_GROUPS
fuente5 <- 'R321C193' # FAO Fisheries and Aquaculture Data Collection. Global Capture Production - File: Capture_Quantity.csv



df_captura <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet(.)

df_acuicola <- argendataR::get_clean_path(fuente1) %>% 
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

df_prod_acuicola <- df_acuicola %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_acuicola = sum(value, na.rm = T)) %>% 
  ungroup()


df_prod_pesquera <- df_captura %>% 
  dplyr::filter(country_un_code %in% un_codes) %>% 
  left_join(df_countries, join_by(country_un_code == un_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  group_by(anio = period, iso3, pais_nombre) %>% 
  summarise(produccion_captura = sum(value, na.rm = T)) %>% 
  ungroup()


df_prod_total_paises <- df_prod_acuicola %>% 
  full_join(df_prod_pesquera, join_by(anio, iso3, pais_nombre)) %>% 
  dplyr::filter(anio>=1961) %>% 
  dplyr::filter(!(is.na(produccion_acuicola) & is.na(produccion_captura))) %>% 
  mutate(
    produccion_acuicola = replace_na(produccion_acuicola, 0),
    produccion_captura = replace_na(produccion_captura, 0),
    produccion_total = produccion_acuicola + produccion_captura
  ) 

df_mundo_prod_total <- df_prod_total_paises %>% 
  group_by(anio) %>% 
  summarise(across(matches("produccion.*"), ~ sum(.x, na.rm = TRUE)), .groups = "drop") %>% 
  mutate(iso3 = "WLD", pais_nombre = "Mundo")


df_prod_total <- df_prod_total_paises %>% 
  bind_rows(df_mundo_prod_total)

df_output <- df_prod_total %>% 
  mutate(share_acuicola = 100*produccion_acuicola / produccion_total) %>% 
  select(anio, iso3, pais_nombre, share_acuicola) %>% 
  drop_na(share_acuicola)


df_output %>%
  argendataR::write_csv_fundar(output_name)

paises_seleccionados <- c("ARG", "BRA", "IND", "CHL", "ECU", "CHN", "WLD")

plot_data <- df_output %>%
  filter(iso3 %in% paises_seleccionados) %>%
  select(anio, pais_nombre, share_acuicola) 


plot_data_labels <- plot_data %>% 
  group_by(pais_nombre) %>% 
  filter(anio == max(anio)) %>% 
  ungroup()

require(ggrepel)
  
ggplot(plot_data, aes(x = anio, y = share_acuicola, color = pais_nombre)) + 
  geom_line() + 
  geom_text_repel(
    data = plot_data_labels,
    aes(label = pais_nombre),
    color = "black",
    nudge_x = 6,  # Mueve las etiquetas hacia la derecha
    direction = "y",  # Evita solapamientos verticales
    hjust = -1,  # Alinea a la izquierda
    segment.color = "#BEBEBF",  # Color de las líneas de conexión
    segment.angle = 90,  # Línea vertical entre la etiqueta y la línea
    size = 2  # Un poco más grande para mejorar visibilidad
  )+
  labs(y = "% producción acuícola", x = "") +
  theme_minimal() +
  theme(
    legend.position = "None",
    axis.text = element_text(color = "black", size = 8),  # Color negro para los números de los ejes
    axis.title = element_text(color = "black", size = 8)  # Color negro para los títulos de los ejes
  )
