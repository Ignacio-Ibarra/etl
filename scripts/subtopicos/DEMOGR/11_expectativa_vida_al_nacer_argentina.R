# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "expectativa_vida_al_nacer_argentina.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R440C285' # INDEC Indicadores Demográficos. Expectativa de vida al nacer: 1869 a 2010
fuente2 <- 'R433C279' # World Population Proscpects - Demographic Indicators. 1950-2100, medium. CSV format

df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)


df_indec_le_arg <- df_indec %>% 
  dplyr::filter(sexo == "Ambos Sexos") %>% 
  rename(valor_fecha = ano) %>% 
  mutate(medida_fecha = "período",
         fuente = "INDEC") %>% 
  select(medida_fecha, valor_fecha, exp_vida_al_nacer, fuente)

df_wpp_le_arg <- df_wpp %>% 
  dplyr::filter(iso3_code == "ARG",
                time <= year(Sys.Date())) %>% 
  mutate(medida_fecha = 'año',
         valor_fecha = as.character(time),
         fuente = "World Population Prospects (UN)") %>% 
  select(valor_fecha, medida_fecha, exp_vida_al_nacer = l_ex, fuente) 


df_output <- df_wpp_le_arg %>% 
  bind_rows(df_indec_le_arg) %>% 
  dplyr::filter() %>% 
  select(valor_fecha, medida_fecha, exp_vida_al_nacer, fuente)



df_plot <- df_output %>%
  mutate(
    fecha_num = case_when(
      medida_fecha == "año" ~ as.integer(valor_fecha),
      medida_fecha == "período" ~ {
        # separar en dos números
        rango <- str_split(valor_fecha, "-", simplify = TRUE)
        anio_ini <- as.integer(rango[, 1])
        anio_fin <- as.integer(rango[, 2])
        ((anio_ini + anio_fin) / 2) |> round()  # punto medio
      }
    )
  ) %>% 
  select(anio = fecha_num, exp_vida_al_nacer) %>% 
  distinct(anio, .keep_all = T) %>% 
  arrange(anio)


ggplot(df_plot, aes(x=anio, y=exp_vida_al_nacer)) + 
  geom_line() + 
  theme_minimal()+
  ylim(0,NA)
