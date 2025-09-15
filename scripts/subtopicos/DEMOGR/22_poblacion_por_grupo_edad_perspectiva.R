# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "poblacion_por_grupo_edad_perspectiva.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R435C280'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_output <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  mutate(poblacion = 1000*pop_total) %>% 
  select(anio = time, rango_etario = age_grp, poblacion) %>% 
  mutate(grupo_edad = case_when(
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) < 20 ~ "menores de 20",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) %>% 
      between(., 20, 64) ~ "20 a 64",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) > 64 | rango_etario == "100+" ~ "65 y más"
    ) %>% factor(., levels = c("65 y más", "20 a 64", "menores de 20"))) %>% 
  group_by(anio, grupo_edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(anio >= year(Sys.Date()))


ggplot(df_output, aes(x = anio, y = poblacion, fill = grupo_edad))  +
  geom_area(alpha = 0.7, color = "black", size = 0.2) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c", "#ff6c1a")) +
  labs(
    title = "",
    x = "",
    y = "",
    fill = ""
  ) +
  theme_minimal() 
