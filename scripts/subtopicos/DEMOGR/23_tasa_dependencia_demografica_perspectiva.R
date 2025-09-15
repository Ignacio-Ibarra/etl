# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_dependencia_demografica_perspectiva.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R435C280'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


df_output <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  select(anio = time, rango_etario = age_grp, V = pop_male, M = pop_female) %>% 
  pivot_longer(cols = c(V, M), names_to = "sexo", values_to = "poblacion_wpp" , values_transform = ~ .x *1000) %>%
  mutate(productiva_bool = case_when(
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) < 20 ~ "no_productiva",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) %>% 
      between(., 20, 64) ~ "productiva",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) > 64 | rango_etario == "100+" ~ "no_productiva")) %>% 
  group_by(anio, productiva_bool) %>% 
  summarise(poblacion = sum(poblacion_wpp, na.rm = T)) %>% 
  ungroup() %>% 
  pivot_wider(id_cols = anio, names_from = productiva_bool, values_from = poblacion) %>% 
  mutate(tasa_dependencia = 100 * no_productiva / productiva) %>% 
  select(anio, tasa_dependencia) %>% 
  dplyr::filter(anio >= year(Sys.Date()))


ggplot(df_output , aes(x=anio, y = tasa_dependencia)) +
  geom_line() +
  theme_minimal() + 
  ylim(0, NA)
