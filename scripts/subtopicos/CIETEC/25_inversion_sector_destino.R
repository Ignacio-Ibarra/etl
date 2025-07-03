# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "CIETEC"
output_name <- "25_inversion_sector_destino.csv"
analista <- "Ignacio Ibarra"
# Defino las fuentes
fuente1 <- 'R407C258' # DNIC Sistema Integrado de Indicadores. Inversión en I+D según sector de ejecución y destino de erogación


df_dnic <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_output <- df_dnic %>% 
  dplyr::filter(anio == max(anio),
                !grepl("Total.*", destino)) %>% 
  group_by(anio, tipo) %>% 
  mutate(
    share = 100*inversion_i_d / sum(inversion_i_d)
  ) %>% 
  ungroup()


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )

library(viridis)


orden_tipo <- df_output %>%
  dplyr::filter(tipo != "Nacional (total)") %>% 
  mutate(destino = as.character(destino)) %>%
  filter(destino == "Erogaciones en personal") %>%
  arrange(-share) %>%
  pull(tipo) %>% 
  c("Nacional (total)", .)

plot_data <- df_output %>% 
  # dplyr::filter(tipo != "Nacional (total)") %>% 
  mutate(
    tipo = factor(tipo, levels = orden_tipo),
    destino = factor(destino, levels = c(
      "Erogaciones en personal",
      "Otras erogaciones corrientes",
      "Erogaciones en equipamiento y rodados",
      "Erogaciones en inmuebles y construcciones",
      "Otras erogaciones de capital"
    ))
  )




umbral <- 5  # ajustalo si necesario

ggplot(plot_data, aes(x = tipo, y = share, fill = destino)) + 
  geom_col() +
  geom_text(
    aes(label = ifelse(share >= umbral, paste(round(share, 1),"%"), "")),
    position = position_stack(vjust = 0.5),
    color = "white",
    size = 3
  ) +
  theme_minimal() +
  labs(y = "Personal en I+D", x = "") +
  theme(
    legend.position = "bottom", 
    legend.title = element_blank(),
    axis.text = element_text(color = "black"),  
    axis.title = element_text(color = "black")
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  scale_fill_viridis_d(option = "H", direction = -1, alpha = 0.8)

