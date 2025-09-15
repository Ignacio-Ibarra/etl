# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "poblacion_edad_jubilatoria.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R449C290'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_varones <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG", 
                age_grp == 65) %>% 
  mutate(categoria = glue::glue("Varones de {age_grp} años"),
         poblacion = 1000*pop_male) %>%
  select(anio = time, categoria, poblacion)


df_mujer <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG", 
                age_grp == 60) %>% 
  mutate(categoria = glue::glue("Mujeres de {age_grp} años"),
         poblacion = 1000*pop_female) %>%
  select(anio = time, categoria, poblacion)
  

df_output <- bind_rows(df_varones, df_mujer)


ggplot(df_output, aes(anio, poblacion, color = categoria)) + 
  geom_line() + 
  theme_minimal()
