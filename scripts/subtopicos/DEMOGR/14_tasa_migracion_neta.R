# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_migracion_neta.csv"
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


df_migracion_lattes <- df_lattes_cuadro22 %>%
  mutate(saldo_migratorio = 1000*migratorio_miles,
         anio = as.integer(str_extract(periodo, "^[0-9]{4}"))) %>% 
  select(anio, saldo_migratorio) 


df_poblacion_lattes <- df_lattes_cuadro21 %>% 
  mutate(poblacion_media = 1000*(poblacion_total + lead(poblacion_total))/2)  %>% 
  select(anio, poblacion_media) 

df_tasa_migracion_lattes <- df_migracion_lattes %>% 
  left_join(df_poblacion_lattes, join_by(anio)) %>% 
  mutate(tasa_migracion_neta = 0.2*100* saldo_migratorio / poblacion_media, 
         fuente = "Lattes et al (1975)") %>% 
  select(anio, tasa_migracion_neta)


df_wpp_mrate <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  select(anio = time, pob = t_population1july, net_migrations) %>% 
  mutate(tasa_migracion_neta = 100 * net_migrations / pob,
         fuente = "World Population Prospects (UN)") %>% 
  select(anio, tasa_migracion_neta, fuente)
  


df_output <- df_tasa_migracion_lattes %>% 
  bind_rows(df_wpp_mrate)


ggplot(df_output, aes(anio, tasa_migracion_neta)) + geom_line() + theme_minimal()
