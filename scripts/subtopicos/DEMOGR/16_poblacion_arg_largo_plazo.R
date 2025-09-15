# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "poblacion_arg_largo_plazo.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R219C90' # Maddison
fuente2 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format
fuente3 <- 'R444C0' # Lattes

df_madd <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_lattes <- argendataR::get_raw_path(fuente3) %>% 
  read.csv

df_madd_arg <- df_madd %>% 
  dplyr::filter(iso3 == "ARG") %>% 
  mutate(fuente = "Maddison Project Database" ) %>% 
  select(anio, poblacion = pop, fuente) %>% 
  drop_na(poblacion)

df_wpp_arg <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  mutate(poblacion = 1000*t_population1july,
         fuente = "World Population Prospects (UN)") %>% 
  select(anio = time, poblacion, fuente)

df_lattes_pob <- df_lattes %>% 
  mutate(
    anio = str_extract(quinquenio, "\\d{4}") %>% as.integer(),
    poblacion = poblacion_inicial,
    fuente = "Lattes et al (1975)") %>% 
  select(anio, poblacion, fuente)


df_output <- df_madd_arg %>% 
  dplyr::filter(anio < min(df_lattes_pob$anio)) %>% 
  bind_rows(
    df_lattes_pob %>% 
      dplyr::filter(anio < min(df_wpp_arg$anio))
  ) %>% 
  bind_rows(df_wpp_arg)


ggplot(df_output, aes(x=anio, y = poblacion)) + geom_line() + theme_minimal()
