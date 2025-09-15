# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "mediana_edad_comparativo.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R433C279'

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_wpp %>% 
  select(anio = time, geocodigoFundar = iso3_code, edad_mediana = median_age_pop) %>% 
  dplyr::filter(!is.na(geocodigoFundar), trimws(geocodigoFundar) != '', !is.na(edad_mediana)) %>% 
  left_join(geo_front, join_by(geocodigoFundar))


# df_plot <- df_output %>% 
#   dplyr::filter(geonombreFundar %in% c("Argentina", "Chile", "Perú", "Uruguay", "Francia", "España"),
#                 anio <= year(Sys.Date()))
# 
# 
# ggplot(df_plot, aes(x=anio, y=edad_mediana, color = geonombreFundar)) +
#   geom_line() + 
#   theme_minimal() + 
#   ylim(0,NA)


