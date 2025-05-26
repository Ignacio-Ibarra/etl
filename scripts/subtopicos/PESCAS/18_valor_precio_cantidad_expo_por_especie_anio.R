#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "18_valor_precio_cantidad_expo_por_especie_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R324C197' # FAO Global Aquatic Trade - All parteners aggregated - Trade Value
fuente2 <- 'R324C198' # FAO Global Aquatic Trade - All parteners aggregated - Trade Quantity
fuente3 <- 'R324C199' # FAO Global Aquatic Trade - All parteners aggregated - Commodity ISSCFC


df_valores <- argendataR::get_clean_path(fuente1) %>%
  arrow::read_parquet(.) %>% 
  dplyr::filter(country_reporter_un_code == "32", trade_flow_alpha_code == "E") %>% 
  select(-country_reporter_un_code, -trade_flow_alpha_code)

df_cantidades <- argendataR::get_clean_path(fuente2) %>%
  arrow::read_parquet(.) %>% 
  dplyr::filter(country_reporter_un_code == "32", trade_flow_alpha_code == "E") %>% 
  select(-country_reporter_un_code, -trade_flow_alpha_code)

df_comodity <- argendataR::get_clean_path(fuente3) %>%
  arrow::read_parquet(.) %>% 
  select(code, isscfc_parent, name_es)


df_expo_arg <- df_valores %>% 
  left_join(df_cantidades, join_by(commodity_fao_code, period), suffix = c("_valores","_cantidades")) %>% 
  left_join(df_comodity, join_by(commodity_fao_code == code))

df_output <- df_expo_arg %>% 
  mutate(
    especie = case_when(
      grepl("(.*langostino.*|.*Langostino.*|.*camarones.*|.*Camarones.*)", name_es) ~ "Langostino",
      # commodity_fao_code == "036.0.1.4.5.60" ~ "Langostino",
      grepl("(.*Calamares.*Illex.*|.*calamares.*illex.*|.*calamares.*|.*Calamar.*|.*calamar.*)", name_es) ~ "Calamar", 
      # commodity_fao_code == "036.0.2.4.7.40" ~ "Calamar", 
      commodity_fao_code %in% c("034.3.1.5.2.63","034.4.1.5.2.63","034.1.5.2.63","034.2.5.2.63") ~ "Merluza Hubbsi",
      TRUE ~ "Otras especies"
      )
  ) %>% 
  group_by(anio = period, especie) %>%
  summarise(
    valores = sum(value_valores, na.rm = T),
    cantidades = sum(value_cantidades, na.rm = T),
    precio = valores / cantidades
  ) %>%
  ungroup() %>% 
  dplyr::filter(anio>=2002) %>% 
  group_by(anio) %>% 
  dplyr::filter(n() == 4) %>% 
  ungroup() %>% 
  arrange(anio, especie) %>% 
  mutate(
    indice_valores = 100 * valores / valores[anio == min(anio)],
    indice_toneladas = 100 * cantidades / cantidades[anio == min(anio)],
    indice_precio = 100 * precio / precio[anio == min(anio)]
  ) %>% select(anio, especie, starts_with("indice_")) %>%
  pivot_longer(starts_with("indice_"), 
               names_to = "variable",
               names_transform = function(x){str_remove(x, "indice_") %>% tools::toTitleCase(.)},
               values_to = "valor") 


df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


library(ggrepel)

plot_data_labels <- df_output %>%
  group_by(especie, variable) %>%
  filter(anio == max(anio)) %>%
  ungroup()

ggplot(df_output, aes(x = anio, y = valor, color = factor(variable))) + 
  geom_line() +
  ylim(c(0,NA))+
  geom_text_repel(
    data = plot_data_labels,
    aes(label = variable),
    color = "black",
    nudge_x = 6,  # Mueve las etiquetas hacia la derecha
    direction = "y",  # Evita solapamientos verticales
    hjust = -1,  # Alinea a la izquierda
    segment.color = "#BEBEBF",  # Color de las líneas de conexión
    segment.angle = 90,  # Línea vertical entre la etiqueta y la línea
    size = 2  # Un poco más grande para mejorar visibilidad
  ) +
  labs(y = "Índice", x = "", color = "Variable") +
  scale_color_viridis_d(option = "plasma") +
  scale_x_continuous(breaks = pretty(df_output$anio, n = 4)) +
  theme_minimal()+
  theme(
    axis.text = element_text(color = "black", size = 8),  
    axis.title = element_text(color = "black", size = 8),
    legend.position = "none"
  )+
  facet_wrap(~especie, nrow=2, scales = "free_y")
  
  


