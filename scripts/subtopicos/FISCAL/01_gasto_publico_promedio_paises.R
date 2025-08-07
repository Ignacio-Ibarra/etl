# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_promedio_paises.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"

fuente1 <- 'R424C272'
fuente2 <- 'R325C200'


df_imf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_mecon <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_arg <- df_mecon %>% 
  dplyr::filter(nombre_apertura == "GASTO PÚBLICO TOTAL", anio>=2014) %>% 
  summarise(gasto_pub_gdp = mean(valores, na.rm=T)) %>% 
  mutate(iso3 = 'ARG', 
         pais_nombre = 'Argentina', 
         fuente = "MECON") %>% 
  select(iso3, pais_nombre, gasto_pub_gdp, fuente)


df_output <- df_imf %>% 
  dplyr::filter(anio>=2014) %>% 
  group_by(iso3, pais_nombre) %>% 
  summarise(
    gasto_pub_gdp = mean(exp, na.rm = T)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  mutate(fuente = "FMI") %>% 
  bind_rows(df_arg) %>% 
  arrange(-gasto_pub_gdp)





