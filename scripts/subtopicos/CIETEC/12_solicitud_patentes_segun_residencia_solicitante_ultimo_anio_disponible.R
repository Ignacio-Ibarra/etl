# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "12_solicitud_patentes_segun_residencia_solicitante_ultimo_anio.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R405C256' # WIPO Patent. 1 - Total patent applications (direct and PCT national phase entries). Total count by applicant&#39;s origin. 1980 - 2023.


# Lectura de archivos Parquet
df_wipo <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_output <- df_wipo %>% 
  dplyr::filter(metrica == "Total") %>% 
  mutate(anio = as.integer(anio)) %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  select(anio, iso3, pais = nombre_pais, patentes_de_residentes = valor) 



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
  "Perú",
  "Uruguay",
  "Ecuador"
)

plot_data <- df_output %>% 
  dplyr::filter(pais %in% paises_seleccionados) %>%
  arrange(patentes_de_residentes) %>% 
  mutate(pais = factor(pais, levels = unique(pais)))

regular_texto <- 2

ggplot(plot_data, aes(x = patentes_de_residentes, y = pais, 
                      fill = case_when(
                        pais == "Argentina" ~ "Argentina",
                        # pais %in% c("América Latina y el Caribe") ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", 
                               # "Especiales" = "#404B84", 
                               "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = patentes_de_residentes ,x = patentes_de_residentes + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 4) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
