#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "30_share_global_expo_pesca_pais_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R397C0' # Atlas SITC

df_sitc <- argendataR::get_raw_path(fuente1) %>%
  read_csv()


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>%
  select(iso3 = geocodigo, pais_nombre = name_long)

df_output <- df_sitc %>% 
  dplyr::filter(grepl("^03.*", product_sitc_code)) %>% 
  group_by(anio = year, iso3 = country_iso3_code) %>% 
  summarise(
    expo_pesca = sum(export_value)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(
    share_global = 100*expo_pesca / sum(expo_pesca, na.rm = T)
  ) %>% 
  ungroup() %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(
    pais_nombre = ifelse(iso3 == "YUG", "Yugoslavia", pais_nombre)
  )



df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


paises_seleccionados <- c("ARG", "CHL", "VNM")

plot_data <- df_output %>%
  filter(iso3 %in% paises_seleccionados) %>%
  select(anio, pais_nombre, share_global) 


plot_data_labels <- plot_data %>% 
  group_by(pais_nombre) %>% 
  filter(anio == max(anio)) %>% 
  ungroup()

require(ggrepel)

ggplot(plot_data, aes(x = anio, y = share_global, color = pais_nombre)) + 
  geom_line() + 
  geom_text_repel(
    data = plot_data_labels,
    aes(label = pais_nombre),
    color = "black",
    nudge_x = 6,  # Mueve las etiquetas hacia la derecha
    direction = "y",  # Evita solapamientos verticales
    hjust = -1,  # Alinea a la izquierda
    segment.color = "#BEBEBF",  # Color de las líneas de conexión
    segment.angle = 90,  # Línea vertical entre la etiqueta y la línea
    size = 2  # Un poco más grande para mejorar visibilidad
  )+
  labs(y = "% participación global", x = "") +
  theme_minimal() +
  theme(
    legend.position = "None",
    legend.title = element_blank(),
    axis.text = element_text(color = "black", size = 8),  # Color negro para los números de los ejes
    axis.title = element_text(color = "black", size = 8)  # Color negro para los títulos de los ejes
  )
