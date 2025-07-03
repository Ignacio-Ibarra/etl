# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "23_patentes_pais_residencia_solicitante_evo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R405C256' # WIPO Patent. 1 - Total patent applications (direct and PCT national phase entries). Total count by applicant&#39;s origin. 1980 - 2023.


# Lectura de archivos Parquet
df_wipo <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_output <- df_wipo %>% 
  dplyr::filter(metrica == "Total") %>% 
  mutate(anio = as.integer(anio)) %>% 
  select(anio, iso3, nombre_pais, patentes_de_residentes = valor)


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Chile",
  "Brasil",
  "México",
  "Colombia",
  "Turquía",
  "Tailandia",
  "Nueva Zelandia",
  "Sudáfrica"
)



plot_data <- df_output %>% 
  dplyr::filter(nombre_pais %in% paises_seleccionados)

plot_data_labels <- plot_data %>% 
  group_by(nombre_pais) %>% 
  filter(anio == max(anio)) %>% 
  ungroup()


require(ggrepel)

ggplot(plot_data, aes(x = anio, 
                      y = patentes_de_residentes, 
                      group = nombre_pais)) + 
  geom_line(linewidth = 0.8, aes(color = nombre_pais)) + 
  scale_x_continuous(breaks = pretty(plot_data$anio))+
  geom_text_repel(
    data = plot_data_labels,
    aes(label = nombre_pais),
    color = "black",
    nudge_x = 6,
    direction = "y",
    hjust = -1,
    segment.color = "#BEBEBF",
    segment.angle = 90,
    size = 3
  ) +
  labs(y = "Cantidad de patentes", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
