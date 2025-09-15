# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "edad_media_maternidad.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format


df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_wpp %>% 
  dplyr::filter(!is.na(iso3_code),
                iso3_code != "") %>%
  select(time, iso3_code, mac) %>% 
  left_join(geo_front, join_by(iso3_code == geocodigoFundar)) %>% 
  select(anio = time, geocodigoFundar = iso3_code, geonombreFundar, edad_media_maternidad = mac)






# df_plot <- df_output %>% 
#   dplyr::filter(geonombreFundar %in% c("Argentina", "Chile", "Perú", "Uruguay", "Francia", "España", "México"),
#                 anio <= year(Sys.Date()))
# 
# 
# ggplot(df_plot, aes(x=anio, y=edad_media_maternidad, color = geonombreFundar)) +
#   geom_line() + 
#   theme_minimal()

