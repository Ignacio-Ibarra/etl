# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "24_participacion_fem_investigadores_becarios_comparativo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R365C240' # UNESCO Headcount (HC) of R&D personnel


df_unesco_personas <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  select(iso3  = geoUnit, anio = year , valor = value)


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long) 

df_paises <- df_unesco_personas  %>% 
  inner_join(geo_front, join_by(iso3))


df_no_paises <- df_unesco_personas %>% 
  anti_join(df_paises, join_by(iso3)) %>% 
  dplyr::filter(grepl("(WB:|MDG: Latin America)", iso3)) %>% 
  mutate(iso3 = case_when(
    grepl("Latin America", iso3) ~ "LCN",
    grepl("High income", iso3) ~ "HIC",
    grepl("Low income", iso3) ~ "LIC",
    grepl("Middle income", iso3) ~ "MIC",
    grepl("Upper middle income", iso3) ~ "UMC",
    grepl("Lower middle income", iso3) ~ "LMC",
    grepl("World", iso3) ~ "WLD"
  )
  ) %>% 
  left_join(geo_front, join_by(iso3))


df_output <- bind_rows(df_paises, df_no_paises) %>% 
  rename(share_mujeres = valor) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  select(ultimo_anio_disponible = anio, iso3, pais_nombre, share_mujeres) %>% 
  arrange(-share_mujeres)


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Uruguay",
  "Mundo", 
  "Perú",
  "Alemania",
  "América Latina y el Caribe",
  "Países de ingreso medio",
  "Países de altos ingresos",
  "Mundo",
  "Francia",
  "Portugal",
  "Suiza"
)

plot_data <- df_output %>% 
  mutate(pais_anio = paste(pais_nombre, "(", ultimo_anio_disponible, ")")) %>% 
  mutate(pais_anio = factor(pais_anio, levels = rev(unique(pais_anio)))) %>% 
  dplyr::filter(pais_nombre %in% paises_seleccionados)

regular_texto <- 0.5

ggplot(plot_data, aes(x = share_mujeres, y = pais_anio, 
                      fill = case_when(
                        pais_nombre == "Argentina" ~ "Argentina",
                        pais_nombre %in% c("Países de ingreso medio",
                                           "Países de altos ingresos",
                                           "América Latina y el Caribe",
                                           "Mundo") ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "#404B84", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = round(share_mujeres,1),x = share_mujeres + regular_texto),  
    vjust = 0, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "Porcentaje de mujeres") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
