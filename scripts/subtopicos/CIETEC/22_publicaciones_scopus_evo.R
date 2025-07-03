# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "22_publicaciones_scopus_evo.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R363C237' # RICYT Publicaciones en SCOPUS

# Lectura de archivos Parquet
df_ricyt <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long) 

america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")


df_output <- df_ricyt %>% 
  dplyr::filter(pais != "Total") %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Haiti" ~ "HTI",
    pais == "Iberoamérica" ~ "RICYT_IBE",
    pais == "República Dominicana" ~ "DOM",
    TRUE ~ iso3
  ))



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c(
  "Argentina",
  "Chile",
  "Brasil",
  "México",
  "Colombia"
)



plot_data <- df_output %>% 
  dplyr::filter(pais %in% paises_seleccionados)

plot_data_labels <- plot_data %>% 
  group_by(pais) %>% 
  filter(anio == max(anio)) %>% 
  ungroup()


require(ggrepel)

ggplot(plot_data, aes(x = anio, 
                      y = valor, 
                      color = pais)) + 
  geom_line(linewidth = 0.8) + 
  geom_text_repel(
    data = plot_data_labels,
    aes(label = pais),
    color = "black",
    nudge_x = 6,
    direction = "y",
    hjust = -1,
    segment.color = "#BEBEBF",
    segment.angle = 90,
    size = 3
  ) +
  labs(y = "Publicaciones en SCOPUS", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black")
  )
