# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_crecimiento_componentes.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R442C0' # Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentina. Cuadro 2.2
fuente2 <- 'R443C0' # Lattes, A. E., Recchini de Lattes, Z. L. (1975). La Población de Argentin. Cuadro 2.1
fuente3 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format


df_lattes_cuadro22<- argendataR::get_raw_path(fuente1) %>% 
  read.csv(.)

df_lattes_cuadro21 <- argendataR::get_raw_path(fuente2) %>% 
  read.csv(.)

df_wpp <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()


df_saldos_lattes <- df_lattes_cuadro22 %>%
  mutate(saldo_migratorio_medio = 1000*migratorio_miles/5,
         saldo_vegetativo_medio = 1000*vegetativo_miles/5,
         anio = as.integer(str_extract(periodo, "^[0-9]{4}"))) %>% 
  select(anio, saldo_migratorio_medio, saldo_vegetativo_medio) 


df_poblacion_lattes <- df_lattes_cuadro21 %>% 
  mutate(poblacion_mediagregar_fuente_raw(url = url,
                                          institucion = institucion,a = 1000*(poblacion_total + lead(poblacion_total))/2)  %>% 
  select(anio, poblacion_media) 

df_lattes_rates <- df_saldos_lattes %>% 
  left_join(df_poblacion_lattes, join_by(anio)) %>% 
  mutate(tasa_migracion_neta = 100* saldo_migratorio_medio / poblacion_media, 
         tasa_crecimiento_vegetativo = 100 * saldo_vegetativo_medio / poblacion_media,
         fuente = "Lattes et al (1975)") %>% 
  select(anio, tasa_migracion_neta,tasa_crecimiento_vegetativo)


df_wpp_rates <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  select(anio = time, pob = t_population1july, net_migrations, nat_change, pop_change) %>% 
  mutate(
    tasa_migracion_neta = 100 * net_migrations / pob,
    tasa_crecimiento_vegetativo = 100 * nat_change / pob
  ) %>% 
  select(anio, tasa_migracion_neta, tasa_crecimiento_vegetativo)


df_output <- df_lattes_rates %>% 
  dplyr::filter(anio < min(df_wpp_rates$anio)) %>% 
  bind_rows(df_wpp_rates)


df_plot <- df_output %>%
  pivot_longer(cols = !anio, names_to = "variable", values_to = "tasa") %>%
  mutate( variable = factor(variable, levels = c("tasa_migracion_neta", 'tasa_crecimiento_vegetativo')))


ggplot(df_plot, aes(x = anio, y = tasa, fill = variable))  +
  geom_area(alpha = 0.7, color = "black", size = 0.2) +
  scale_fill_manual(values = c("#1f78b4", "#33a02c")) +
  labs(
    title = "",
    x = "",
    y = "Tasa",
    fill = ""
  ) +
  theme_minimal() 
