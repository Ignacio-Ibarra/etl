# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "mediana_edad_argentina.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R432C278'
fuente2 <- 'R433C279'

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_censos <- df_indec %>% 
  group_by(anio = censo, sexo, rango_etario = edad) %>% 
  summarise(poblacion = sum(poblacion, na.rm = T)) %>% 
  ungroup() 


df_mediana_censos <- df_censos  %>% 
  mutate(
    Li = as.numeric(str_extract(rango_etario, "^[0-9]+")),          # límite inferior
    Ls = as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")),     # límite superior
    w  = Ls - Li                                                   # ancho del intervalo
  ) %>%
  group_by(anio) %>%
  arrange(Li, .by_group = TRUE) %>%
  mutate(
    N = sum(poblacion),                         # total población por año
    FA = cumsum(poblacion),                     # frecuencia acumulada
    FA_prev = lag(FA, default = 0)              # acumulada anterior
  ) %>%
  # identificar el intervalo que contiene la mediana (N/2)
  filter(FA >= N/2) %>%
  slice(1) %>%
  mutate(
    edad_mediana = Li + ((N/2 - FA_prev) / poblacion) * w
  ) %>%
  select(anio, edad_mediana)

df_mediana_wpp <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG") %>% 
  select(anio = time, edad_mediana = median_age_pop) 

df_output <- df_mediana_censos %>%
  dplyr::filter(anio < 1950) %>% 
  bind_rows(df_mediana_wpp)

anios_completos <- data.frame(anio = min(df_stage$anio):max(df_stage$anio))


df_plot <- anios_completos %>%
  left_join(df_output, join_by(anio)) %>% 
  arrange(anio) %>% 
  mutate(
    edad_mediana_interpolada = stats::approx(
      x = anio[!is.na(edad_mediana)],  # años conocidos
      y = edad_mediana[!is.na(edad_mediana)],  # valores conocidos
      xout = anio,                  # todos los años
      method = "linear",
      rule = 2                      # 2 = no extrapola fuera de rango, repite extremos
    )$y
  ) 
  


# ggplot(df_output, aes(x = anio, y = edad_mediana_interpolada)) + geom_line() + theme_minimal() + ylim(0,NA)

