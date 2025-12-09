# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

library(tidytext)

# Defino variables
subtopico <- "TURISM"
output_name <- "concentracion_internos_receptivo_top10_destinos_argentinos.csv"
analista <- "Carola della Paolera & Joan Manuel Vezzato"

fuente1 <- 'R480C0'


# Leer archivos
df_yvera <- argendataR::get_raw_path(fuente1) %>% 
  read.csv()


df_intermediate <- df_yvera %>%
  mutate(anio = year(indice_tiempo)) %>% 
  dplyr::filter(anio >= 2018, anio <= 2024) %>%
  select(matches("indice_tiempo|^viaj_loc_.*_(residentes|no_residentes)$")) %>% 
  pivot_longer(
    !all_of("indice_tiempo"),
    names_to = "serie",
    values_to = "viajeros"
  ) %>%
  mutate(
    residencia = if_else(str_detect(serie, "_no_residentes$"),
                         "no_residentes", "residentes"),
    localidad  = serie |>
      str_remove("^viaj_loc_") |>
      str_remove("_(residentes|no_residentes)$") |>
      str_replace_all("_", " ") |>
      stringr::str_to_title(locale = "es")
  ) 


df_output <-  df_intermediate %>%
  group_by(residencia, localidad) %>%
  summarise(suma_2018_2024 = sum(viajeros, na.rm = TRUE),
            .groups = "drop") %>% 
  group_by(residencia) %>% 
  mutate(
    share = 100 * suma_2018_2024 / sum(suma_2018_2024),
    top_10 = ifelse(rank(-share)<=10, "Top 10", "Resto destinos")
  ) %>% 
  ungroup() %>% 
  select(residencia, 
         localidad, 
         share,
         top_10
         )
# 
# 
# df_plot_resto <- df_output %>% 
#   dplyr::filter(top_10 == "Resto destinos") %>% 
#   mutate(localidad = "Resto localidades") %>% 
#   group_by(residencia, localidad, top_10) %>%
#   summarise(
#     share = sum(share, na.rm = T)
#   ) %>% 
#   ungroup()
# 
# 
# df_plot_top10 <- df_output %>% 
#   dplyr::filter(top_10 == "Top 10") %>% 
#   group_by(residencia) %>% 
#   arrange(-share) %>% 
#   ungroup() 
# 
# 
# df_plot <- df_plot_top10 %>% 
#   bind_rows(df_plot_resto) %>% 
#   group_by(residencia) %>% 
#   mutate(cumsum_share = cumsum(share)) %>% 
#   ungroup()
# 
# 
# ggplot(
#   df_plot,
#   aes(
#     x = share,
#     y = reorder_within(localidad, -cumsum_share, residencia),
#     fill = top_10
#   )
# ) +
#   geom_col() +
#   facet_wrap(~ residencia, scales = "free_y") +
#   scale_y_reordered() +
#   labs(x = "Share (%)", y = "Localidad") +
#   theme_minimal()
