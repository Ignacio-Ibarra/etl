# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "13_patentes_residentes_sobre_pib.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R355C228' # WIPO Patent. 9 - Resident applications per 100 billion USD GDP (2017 PPP) (by origin). Total count by applicant&#39;s origin. 1980 - 2023.


df_wipo <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()


df_output <- df_wipo %>% 
  group_by(iso3) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup() %>% 
  arrange(-valor) %>% 
  select(-iso2)



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )

paises_seleccionados <- c(
  "ARG",
  "CHL",
  "BRA",
  "MEX",
  "COL",
  "PER",
  "URY",
  "WLD"
)


df_seleccionados <- df_output %>% 
  dplyr::filter(iso3 %in% paises_seleccionados)

df_top10 <- df_output %>% 
  dplyr::filter(!(iso3 %in% paises_seleccionados)) %>% 
  slice_head(n = 10)

plot_data <- bind_rows(df_top10, df_seleccionados) %>% 
  arrange(-valor) %>% 
  mutate(pais_label = factor(stringr::str_pad(nombre_pais, width = 10), levels = rev(stringr::str_pad(unique(nombre_pais), width = 10)))) 

regular_texto <- 30

ggplot(plot_data, aes(x = valor, y = pais_label, 
                      fill = case_when(
                        nombre_pais == "Argentina" ~ "Argentina",
                        nombre_pais %in% c("Mundo") ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "#404B84", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = round(valor,1),x = valor + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 3) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
