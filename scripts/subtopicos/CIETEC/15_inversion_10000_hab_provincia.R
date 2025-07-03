# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "15_inversion_10000_hab_provincia.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R356C231' # DNIC Sistema Integrado de Indicadores. Inversión en I+D por jurisdicción. Años 2004 – 2023
fuente2 <- 'R389C242' # INDEC Población estimada al 1 de julio según año calendario por sexo para el total del país y provincias. Años 2010-2040


df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_indec <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

id_provincias <- argendataR::get_nomenclador_geografico_front()  %>% 
  dplyr::filter(grepl("AR-\\w$", geocodigo)) %>% 
  select(provincia_id = geocodigo, provincia = name_long)


df_inversion <- df_dnic %>% 
  dplyr::filter(anio == max(anio)) %>% 
  mutate(
    provincia = case_when(
      provincia == "Ciudad de Buenos Aires" ~ "CABA",
      provincia == "Río  Negro" ~ "Río Negro",
      TRUE ~ provincia
    )
  ) %>% 
  left_join(id_provincias, join_by(provincia)) %>% 
  select(anio, provincia_id, provincia, inversion_i_d = inversion_en_i_d_en_millones_de_pesos_corrientes) %>% 
  mutate(provincia_lower = tolower(provincia))


df_poblacion <- df_indec %>% 
  dplyr::filter(sexo == "Ambos sexos") %>% 
  mutate(provincia_lower = tolower(provincia)) %>% 
  mutate(anio = as.integer(anio)) %>% 
  select(anio, provincia_lower, poblacion_proyectada)

df_output <- df_inversion %>% 
  left_join(df_poblacion, join_by(anio, provincia_lower)) %>% 
  mutate(inversion_10000_hab = 10000*inversion_i_d / poblacion_proyectada,
         indice_inversion_10000_hab = 100*inversion_10000_hab/mean(inversion_10000_hab)) %>% 
  select(anio, provincia_id, provincia, inversion_10000_hab, indice_inversion_10000_hab)



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- df_output %>% select(provincia, indice_inversion_10000_hab) %>% 
  arrange(indice_inversion_10000_hab) %>% 
  mutate(provincia = factor(provincia, levels = unique(provincia)))


regular_texto <- 2

ggplot(plot_data, aes(x = indice_inversion_10000_hab, y = provincia, 
                      fill = case_when(
                        TRUE ~ "color"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("color" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = round(indice_inversion_10000_hab,2),x = indice_inversion_10000_hab + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
