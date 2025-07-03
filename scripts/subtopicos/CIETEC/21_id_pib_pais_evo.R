# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "21_id_pib_pais_evo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R343C218' # UNESCO GERD as a percentage of GDP

# Lectura de archivos Parquet
df_unesco <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  rename(iso3 = geoUnit, anio = year, gerd_gdp = value)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais_nombre = name_long) 

df_paises <- df_unesco  %>% 
  inner_join(geo_front, join_by(iso3)) %>% 
  mutate(nivel_agregacion = "pais") %>% 
  select(anio, iso3, pais_nombre, gerd_gdp, nivel_agregacion)


df_no_paises <- df_unesco %>% 
  anti_join(df_paises, join_by(iso3)) %>% 
  dplyr::filter(grepl("WB:", iso3)) %>% 
  mutate(iso3 = case_when(
    
    grepl("High income", iso3) ~ "HIC",
    grepl("Low income", iso3) ~ "LIC",
    grepl("Middle income", iso3) ~ "MIC",
    grepl("Upper middle income", iso3) ~ "UMC",
    grepl("Lower middle income", iso3) ~ "LMC",
    grepl("World", iso3) ~ "WLD"
  )
  ) %>% 
  left_join(geo_front, join_by(iso3)) %>% 
  mutate(nivel_agregacion = "agregacion") %>% 
  select(anio, iso3, pais_nombre, gerd_gdp, nivel_agregacion)


df_output <- bind_rows(df_paises, df_no_paises) 


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Chile",
  "Perú",
  "Brasil",
  "México",
  "Uruguay",
  "Austria",
  "China",
  "Colombia", 
  "Rusia",
  "India",
  "Países de ingreso medio",
  "Países de altos ingresos",
  "Mundo",
  "Canadá",
  "España",
  "Estados Unidos",
  "Japón",
  "Italia",
  "Bélgica",
  "Alemania",
  "Israel",
  "Corea del Sur",
  "Tailandia"
)



plot_data <- df_output %>% 
  dplyr::filter(pais_nombre %in% paises_seleccionados)

plot_data_labels <- plot_data %>% 
  group_by(pais_nombre) %>% 
  filter(anio == max(anio)) %>% 
  ungroup()

especiales <- df_output %>% 
  group_by(iso3) %>% 
  dplyr::filter(n()>20) %>% 
  dplyr::filter(anio == max(anio) | anio==min(anio)) %>% 
  mutate(punta = ifelse(anio == min(anio), "anio_min", "anio_max")) %>% 
  ungroup() %>% 
  select(-anio) %>% 
  pivot_wider(id_cols = c(iso3, pais_nombre, nivel_agregacion), 
              names_from = punta,
              values_from = gerd_gdp) %>% 
  mutate(relative = anio_max/anio_min) %>% 
  arrange(-relative) %>% 
  slice_head(n = 30)

especiales <- c("China", "Bélgica", "Israel", "Corea del Sur", "Tailandia")

require(ggrepel)

ggplot(plot_data, aes(x = anio, 
                      y = gerd_gdp, 
                      group = pais_nombre)) + 
  geom_line(linewidth = 0.8, aes(color = case_when(
    pais_nombre == "Argentina" ~ "Argentina",
    pais_nombre %in% especiales ~ "Especiales",
    TRUE ~ "Otros"
  ))) + 
  scale_color_manual(values = c("Argentina" = "darkblue", "Especiales" = "darkorange", "Otros" = "#A8A8A8")) +
  geom_text_repel(
    data = plot_data_labels,
    aes(label = pais_nombre, color = case_when(
      pais_nombre == "Argentina" ~ "Argentina",
      pais_nombre %in% especiales ~ "Especiales",
      TRUE ~ "Otros"
    )),
    nudge_x = 6,
    direction = "y",
    hjust = -1,
    segment.color = "#BEBEBF",
    segment.angle = 90,
    size = 3
  ) +
  labs(y = "Gasto en I+D en relación al PIB", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )

