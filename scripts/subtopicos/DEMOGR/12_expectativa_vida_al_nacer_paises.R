# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "expectativa_vida_al_nacer_paises.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format

df_wpp <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- df_wpp %>% 
  dplyr::filter(iso3_code != "",
                !is.na(iso3_code),
                time <= year(Sys.Date())) %>% 
  left_join(geo_front, join_by(iso3_code == geocodigoFundar)) %>% 
  select(anio = time, geocodigoFundar = iso3_code, geonombreFundar, exp_vida_al_nacer = l_ex) 



df_plot <- df_output %>% 
  dplyr::filter(geonombreFundar %in% c("Argentina", "Brasil", "Chile", "Perú", "España", "Uruguay"))

ggplot(df_plot, aes(x=anio, y = exp_vida_al_nacer, color = geonombreFundar)) + 
  geom_line() + 
  theme_minimal()+
  ylim(0, NA)

