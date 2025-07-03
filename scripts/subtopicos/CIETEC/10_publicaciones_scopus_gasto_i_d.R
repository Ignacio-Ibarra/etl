# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "10_publicaciones_scopus_gasto_i_d.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R348C223' # RICyT Publicaciones en Scopus en relación al gasto en I+D

df_ricyt <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(iso3 = geocodigo, pais = name_long)


america_no <- c("F5205", "LAC", "TLA", "DESHUM_ZZH.LAC", "DESHUM_AHDI.LAC")

df_output <- df_ricyt %>% 
  dplyr::filter(medida == "Publicaciones en SCOPUS en relacion al Gasto en I+D (cada millón de U$S PPC)") %>% 
  group_by(pais) %>% 
  dplyr::filter(anio == max(anio)) %>% 
  ungroup()  %>% 
  left_join(geo_front %>% dplyr::filter(!(iso3 %in% america_no)), join_by(pais)) %>% 
  mutate(iso3 = case_when(
    pais == "Iberoamérica" ~ "RICYT_IBE",
    pais == "República Dominicana" ~ "DOM",
    TRUE ~ iso3)) %>% 
  arrange(-valor)



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )

paises_seleccionados <- c(
  "Argentina",
  "Estados Unidos",
  "Chile",
  "Brasil",
  "México",
  "Canadá",
  "España",
  "Uruguay",
  "América Latina y el Caribe"
)

plot_data <- df_output %>% 
  mutate(pais = factor(pais, levels = rev(unique(pais)))) %>% 
  dplyr::filter(pais %in% paises_seleccionados)

regular_texto <- 0.05

ggplot(plot_data, aes(x = valor, y = pais, 
                      fill = case_when(
                        pais == "Argentina" ~ "Argentina",
                        pais %in% c("América Latina y el Caribe") ~ "Especiales",
                        TRUE ~ "Otros"
                      ))) + 
  geom_col(color = "black", linewidth = 0.15, position = position_nudge(y = 0.2), width = 0.8) +  
  scale_fill_manual(values = c("Argentina" = "#45bcc5", "Especiales" = "#404B84", "Otros" = "#FC5A0A")) +  # Colores condicionales
  geom_text(
    aes(label = round(valor,1),x = valor + regular_texto),  
    vjust = -0.5, hjust = 0, color = "black", size = 4) +  # Color de las etiquetas en blanco
  labs(y = "", x = "") +
  theme_minimal() +
  theme(
    legend.position = "none",  # Oculta la leyenda
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")  
  )
