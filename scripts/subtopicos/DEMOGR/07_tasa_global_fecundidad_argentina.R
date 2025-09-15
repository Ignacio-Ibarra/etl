# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_global_fecundidad_argentina.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R436C281' # INDEC Indicadores Demográficos. Tasa Global de Fecundidad. Años censales: 1869 a 2022
fuente2 <- 'R438C283' # World Population Proscpects - Fertility. 1950-2100, 5-year age groups. Age-specific Fertility Rate (ASFR). Percent Age-specific Fertility Rate (PASFR). Births (thousands). CSV format
fuente3 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)
fuente4 <- 'R435C280' # World Population Proscpects - Population
  
df_indec_tgf <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp_fertility <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_deis <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_wpp_population <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet(.)

df_wpp_tgf_arg <-df_wpp_fertility %>% 
  dplyr::filter(iso3_code == "ARG", variant == "Medium") %>% 
  select(anio = time, rango_etario = age_grp, asfr) %>% 
  mutate(asfr = asfr / 1000) %>% 
  group_by(anio) %>% 
  summarise(tgf = 5*sum(asfr)) %>% 
  ungroup()

# rm(df_wpp_fertility)

df_deis_nacimientos_edades <- df_deis %>% 
  dplyr::filter(!grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  group_by(anio, rango_etario_id) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() 

# rm(df_deis)

df_wpp_fem_pop_edades_arg <- df_wpp_population %>% 
  dplyr::filter(iso3_code == 'ARG',
                variant == "Medium") %>% 
  select(anio = time, rango_etario = age_grp, pob_fem = pop_female) %>%
  mutate(
    edad_ultima = ifelse(rango_etario == "100+", 100, as.numeric(str_extract(rango_etario, "(?<=-)[0-9]+")))
    ) %>% 
  mutate(
    rango_etario_id = case_when(
      edad_ultima < 15 ~ 1,
      edad_ultima == 19 ~ 2,
      edad_ultima == 24 ~ 3,
      edad_ultima == 29 ~ 4,
      edad_ultima == 34 ~ 5,
      edad_ultima == 39 ~ 6,
      edad_ultima == 44 ~ 7,
      edad_ultima>45 ~ 8
    )
  ) %>% 
  group_by(anio, rango_etario_id) %>% 
  summarise(
    pob_fem = 1000*sum(pob_fem, na.rm = T)
  ) %>% 
  ungroup()
  

df_deis_tgf <- df_deis_nacimientos_edades %>% 
  left_join(df_wpp_fem_pop_edades_arg, join_by(anio, rango_etario_id)) %>% 
  group_by(anio) %>% 
  summarise(tgf = 5*sum(nacimientos / pob_fem)) %>% 
  ungroup()


# rm(df_wpp_population)

df_output <- df_indec_tgf %>%
  mutate(fuente = 'INDEC') %>% 
  dplyr::filter(anio < 1950) %>% 
  bind_rows(df_wpp_tgf_arg %>% 
              mutate(fuente = "World Population Prospects (UN)") %>% 
              dplyr::filter(anio<2005)) %>%
  bind_rows(df_deis_tgf %>% 
              mutate(fuente = "Dirección de Estadísticas e Información en Salud (DEIS)"))


# ggplot(df_output, aes(x =anio, y = tgf)) + geom_line() + theme_minimal() + ylim(0,NA)

