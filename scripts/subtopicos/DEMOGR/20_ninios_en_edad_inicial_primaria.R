# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "ninios_en_edad_inicial_primaria.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R449C290'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_output <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG", 
                age_grp %in% c(3,6)) %>% 
  mutate(poblacion = 1000*pop_total) %>% 
  select(anio = time, edad = age_grp, poblacion) 


ggplot(df_output, aes(anio, poblacion, color = edad)) + 
  geom_line() + 
  theme_minimal()
