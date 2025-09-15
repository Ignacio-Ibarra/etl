# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "poblacion_grupos_etarios.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R432C278'
fuente2 <- 'R435C280'

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_censos <- df_indec %>%
  mutate(grupo_edad = case_when(
    as.numeric(str_extract(edad, "(?<=-)[0-9]+")) < 20 ~ "menores de 20",
    as.numeric(str_extract(edad, "(?<=-)[0-9]+")) %>% 
      between(., 20, 64) ~ "20 a 64",
    as.numeric(str_extract(edad, "(?<=-)[0-9]+")) > 64 | edad == "85+" ~ "65 y más")) %>% 
  group_by(anio = censo, grupo_edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup()


df_wpp_poblacion <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  select(anio = time, rango_etario = age_grp, V = pop_male, M = pop_female) %>% 
  pivot_longer(cols = c(V, M), names_to = "sexo", values_to = "poblacion_wpp" , values_transform = ~ .x *1000) %>%
  mutate(grupo_edad = case_when(
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) < 20 ~ "menores de 20",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) %>% 
      between(., 20, 64) ~ "20 a 64",
    as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")) > 64 | rango_etario == "100+" ~ "65 y más")) %>% 
  group_by(anio, grupo_edad) %>% 
  summarise(poblacion = sum(poblacion_wpp, na.rm = T)) %>% 
  ungroup()


df_output <- df_censos %>% 
  dplyr::filter(anio < min(df_wpp_poblacion$anio)) %>% 
  bind_rows(df_wpp_poblacion) %>% 
  group_by(anio) %>% 
  mutate(share = 100 *poblacion / sum(poblacion)) %>% 
  ungroup()


# ggplot(df_output %>% 
#          dplyr::filter(anio<=2025) , 
#        aes(x = anio, y = share, color= grupo_edad)) + 
#   geom_line() + 
#   theme_minimal()
