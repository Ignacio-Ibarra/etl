#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "21_acuicultura_produccion_arg_evo.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R320C189' # FAO Fisheries and Aquaculture Data Collection. Global Aquaculture Production - File: Aquaculture_Quantity.csv
fuente2 <- 'R321C193' # FAO Fisheries and Aquaculture Data Collection. Global Capture Production - File: Capture_Quantity.csv


df_acuicola <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_captura <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_prod_acuicola <- df_acuicola %>% 
  dplyr::filter(country_un_code == 32) %>% 
  group_by(anio = period) %>% 
  summarise(produccion_acuicola = sum(value, na.rm = T)) %>% 
  ungroup()


df_prod_pesquera <- df_captura  %>% 
  dplyr::filter(country_un_code == 32) %>% 
  group_by(anio = period) %>% 
  summarise(produccion_captura = sum(value, na.rm = T)) %>% 
  ungroup()


df_output <- df_prod_acuicola %>% 
  full_join(df_prod_pesquera, join_by(anio)) %>% 
  dplyr::filter(anio>=1961) %>% 
  dplyr::filter(!(is.na(produccion_acuicola) & is.na(produccion_captura))) %>% 
  mutate(
    produccion_acuicola = replace_na(produccion_acuicola, 0),
    produccion_captura = replace_na(produccion_captura, 0),
    produccion_total = produccion_acuicola + produccion_captura,
    share_acuicola = 100*produccion_acuicola / produccion_total
  ) %>% 
  mutate(iso3 = "ARG", pais_nombre = "Argentina")



df_output %>%
  argendataR::write_csv_fundar(output_name)



ggplot(df_output, aes(x = anio, y = share_acuicola)) + 
  geom_line(color = "#7ab5c5") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),  # Color negro para los números de los ejes
    axis.title = element_text(color = "black")  # Color negro para los títulos de los ejes
  )+
  labs(y = "Participación en la producción pesquera total", x = "") 
