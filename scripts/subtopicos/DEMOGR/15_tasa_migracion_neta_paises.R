# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_migracion_neta_paises.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_wpp %>% 
  dplyr::filter(time <= year(Sys.Date()),
                !is.na(iso3_code),
                trimws(iso3_code) !="") %>% 
  select(anio = time, geocodigoFundar = iso3_code, pob = t_population1july, net_migrations) %>% 
  mutate(tasa_migracion_neta = 1000 * net_migrations / pob) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, tasa_migracion_neta) %>% 
  drop_na(tasa_migracion_neta)


df_plot <- df_output %>% 
  dplyr::filter(geonombreFundar %in% c("Argentina", "Brasil", "Chile", "Perú", "España", "Uruguay"))

ggplot(df_plot, aes(x=anio, y = tasa_migracion_neta, color = geonombreFundar)) + 
  geom_line() + 
  theme_minimal()
