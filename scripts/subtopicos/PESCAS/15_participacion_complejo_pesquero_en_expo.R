#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "15_participacion_complejo_pesquero_en_expo.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R396C0' # Atlas Services
fuente2 <- 'R397C0' # Atlas SITC

df_services<- argendataR::get_raw_path(fuente1) %>%
  read_csv()

df_sitc <- argendataR::get_raw_path(fuente2) %>%
  read_csv()

df_sitc_arg <- df_sitc %>% 
  dplyr::filter(country_iso3_code == "ARG")

df_sitc_arg_pesca <- df_sitc_arg %>% 
  dplyr::filter(grepl("^03.*", product_sitc_code)) %>% 
  group_by(anio = year) %>% 
  summarise(
    total_pesca = sum(export_value)
  ) %>% 
  ungroup()

df_sitc_bienes_arg <- df_sitc_arg %>% 
  group_by(anio = year) %>% 
  summarise(
    total_bienes = sum(export_value)
  ) %>% 
  ungroup()
  

df_services_arg <- df_services %>% 
  dplyr::filter(country_iso3_code == "ARG") %>% 
  group_by(anio = year) %>% 
  summarise(
    total_servicios = sum(export_value)
  ) %>% 
  ungroup()


df_output <- df_sitc_bienes_arg %>% 
  left_join(df_sitc_arg_pesca, join_by(anio)) %>% 
  left_join(df_services_arg, join_by(anio)) %>% 
  mutate(share_bienes = 100*total_pesca / total_bienes,
         share_expo = 100*total_pesca / (total_bienes + total_servicios)) %>% 
  select(anio, share_bienes, share_expo) %>%  
  drop_na(share_bienes)

df_output %>%
  argendataR::write_csv_fundar(.,
                               glue::glue("scripts/subtopicos/{subtopico}_DEV/outputs/{output_name}")
  )


plot_data <- tidyr::pivot_longer(df_output, cols = c("share_bienes", "share_expo"), 
                                 names_to = "variable", values_to = "valor")





ggplot(plot_data, aes(x = anio, y = valor, color = variable)) + 
  geom_line(linewidth = 1) +
  scale_color_manual(values = c("share_bienes" = "#7ab5c5", "share_expo" = "#fc5a0a"),
                     labels = c("Bienes", "Bienes y servicios"),
                     name = "") +
  theme_minimal() +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    legend.position = "bottom",
  ) +
  labs(y = "Participación", x = "")


