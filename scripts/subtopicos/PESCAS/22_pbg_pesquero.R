# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "22_pbg_pesquero.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R221C92' # CEPAL Desagregación provincial del valor agregado bruto de la Argentina, base 2004


df_cepal <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


geo_info <- argendataR::get_nomenclador_geografico_front() %>% 
  dplyr::filter(grepl("^AR-\\w$", geocodigo)) %>% 
  select(geocodigo, provincia = name_long)

df_output <- df_cepal %>% 
  dplyr::filter(sector_de_actividad_economica == "Pesca") %>% 
  group_by(anio) %>% 
  mutate(share = 100*vab_pb / sum(vab_pb, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(provincia_id,anio) %>% 
  dplyr::filter(vab_pb >0) %>% 
  ungroup() %>% 
  select(-sector_de_actividad_economica, -provincia_id, -region) %>% 
  left_join(geo_info, join_by(provincia))

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


provincias_seleccionadas <- df_output %>% 
  dplyr::filter(anio == max(anio)) %>% 
  arrange(-share) %>% 
  slice_head(n=5) %>% 
  pull(provincia)


df_plot1 <- df_output %>% 
  dplyr::filter(provincia %in% provincias_seleccionadas)

ggplot(df_plot1, aes(x = anio, y = share, color = provincia)) +
  geom_line(linewidth = 1) +
  theme_minimal()+
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    legend.text = element_text(size = 8)
  ) +
  labs(y = "Participación en el PIB pesquero", x = "", color = "") 



df_plot2 <- df_output %>% 
  mutate(provincia = ifelse(provincia %in% provincias_seleccionadas, provincia, "Otras provincias")) %>% 
  group_by(anio, provincia) %>% 
  summarise(share = sum(share)) %>% 
  ungroup() %>% 
  arrange(anio, -share) %>% 
  mutate(provincia = factor(provincia, levels = c(provincias_seleccionadas, "Otras provincias")))

ggplot(df_plot2, aes(x = anio, y = share, fill = provincia)) +
  geom_area() + 
  scale_fill_manual(values =  c("Buenos Aires" = "#2e75bc", "Chubut" = "#71b6c6", 
                                "Santa Cruz" = "#5f896e", "Tierra del Fuego" = "#bd0b36", "Río Negro" = "#ddc252", 
                                "Otras provincias" = "#ffa8a6"))+
  theme_minimal()+
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
    legend.title = element_blank(),
    legend.text = element_text(size = 8),
    legend.
  ) +
  labs(y = "Participación en el PIB pesquero", x = "", color = "") 



