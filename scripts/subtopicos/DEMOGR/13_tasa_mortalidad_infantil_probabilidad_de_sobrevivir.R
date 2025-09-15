# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_mortalidad_infantil_probabilidad_de_sobrevivir.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format
fuente2 <- 'R441C286' # World Population Proscpects - Life Tables. 1950-2023, medium. 

df_wpp_dem <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp_life <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

# Infant Mortality Rate (infant deaths per 1,000 live births)
df_imr <- df_wpp_dem %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  select(anio = time, imr) 

rm(df_wpp_dem)

a0 <- 60
a1 <- 90

df_px <- df_wpp_life %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date()),
                sex == "Total",
                age_grp_start %in% c(a0,a1)) %>% 
  select(anio = time, edad = age_grp_start, lx ) %>% 
  group_by(anio) %>% 
  summarise(prob_sobrevivir= 100 * last(lx) / first(lx))

rm(df_wpp_life)
gc()


df_output <- df_imr %>% 
  inner_join(df_px, join_by(anio))

