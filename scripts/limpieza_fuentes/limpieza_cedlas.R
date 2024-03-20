df_fuentes_raw <- fuentes_raw()

df_fuentes_raw_cedlas <-df_fuentes_raw %>% 
  filter(str_detect(nombre, "Indicadores Sociales de Argentina"))
