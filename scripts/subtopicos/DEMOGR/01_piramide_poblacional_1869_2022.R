# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "piramide_poblacional_1869_2022.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R432C278'

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_censos <- df_indec  %>% 
  group_by(anio = censo, sexo, rango_etario = edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup()  %>% 
  mutate(rango_etario = case_when(
    rango_etario == "0-4" ~ "00-04",
    rango_etario == "5-9" ~ "05-09",
    TRUE ~ rango_etario
  ))


df_output <- df_censos %>% 
  group_by(anio) %>% 
  mutate(share = 100 *poblacion / sum(poblacion, na.rm = T)) %>% 
  ungroup() %>% 
  select(anio, rango_etario, sexo, poblacion, share) 
  
# Codigo para generar un png por año
   
# max_share <- max(abs(df_output$share), na.rm = TRUE)
# 
# dir.create("frames", showWarnings = FALSE)
# years <- unique(df_output$anio)
# 
# purrr::walk2(years, seq_along(years), function(year, i) {
#   df_plot <- df_output %>%
#     mutate(share = ifelse(sexo == "V", -share, share)) %>%
#     filter(anio == year)
#   
#   p <- ggplot(df_plot, aes(x = rango_etario, y = share, fill = sexo)) +
#     geom_bar(stat = "identity", width = 0.8) +
#     coord_flip() +
#     scale_y_continuous(
#       labels = function(x) paste0(abs(x), "%"),
#       limits = c(-max_share, max_share),
#       breaks = scales::pretty_breaks(n = 6)
#     ) +
#     labs(x = "Edades", y = "Población", fill = "Sexo", title = year) +
#     theme_minimal()
#   
#   ggsave(sprintf("frames/frame_%04d.png", i), p, width = 6, height = 5, dpi = 120, bg = "white")
# })


