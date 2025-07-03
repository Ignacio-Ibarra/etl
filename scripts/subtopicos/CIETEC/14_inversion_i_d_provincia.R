# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "14_inversion_i_d_provincia.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R356C231' # DNIC Sistema Integrado de Indicadores. Inversión en I+D por jurisdicción. Años 2004 – 2023



df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

id_provincias <- argendataR::get_nomenclador_geografico_front()  %>% 
  dplyr::filter(grepl("AR-\\w$", geocodigo)) %>% 
  select(provincia_id = geocodigo, provincia = name_long)


df_output <- df_dnic %>% 
  group_by(anio) %>% 
  mutate(share = 100*inversion_en_i_d_en_millones_de_pesos_corrientes / sum(inversion_en_i_d_en_millones_de_pesos_corrientes, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(anio == max(anio)) %>% 
  mutate(
    provincia = case_when(
      provincia == "Ciudad de Buenos Aires" ~ "CABA",
      provincia == "Río  Negro" ~ "Río Negro",
      TRUE ~ provincia
    )
  ) %>% 
  left_join(id_provincias, join_by(provincia)) %>% 
  select(anio, provincia_id, provincia, inversion_i_d = inversion_en_i_d_en_millones_de_pesos_corrientes, share)


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- df_output %>% select(provincia, share) %>% 
  arrange(share) %>% 
  mutate(provincia = factor(provincia, levels = unique(provincia)))


regular_texto <- 1

ggplot(plot_data, aes(x = share, y = provincia, 
                      fill = case_when(
                        TRUE ~ "color"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("color" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = paste0(round(share,2),"%"),x = share + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
