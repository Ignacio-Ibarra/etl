# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_global_fecundidad_argentina.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R438C283' # World Population Proscpects - Fertility. 1950-2100, 5-year age groups. Age-specific Fertility Rate (ASFR). Percent Age-specific Fertility Rate (PASFR). Births (thousands). CSV format
fuente2 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)
fuente3 <- 'R435C280' # World Population Proscpects - Population

df_wpp_fertility <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_deis <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet()

df_wpp_population <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)


# TGF 1950-2100 WPP todos los países
df_wpp_tgf <-df_wpp_fertility %>% 
  dplyr::filter(!is.na(iso3_code),
                trimws(iso3_code) != '',
                variant == "Medium") %>% 
  select(anio = time, geocodigoFundar = iso3_code, rango_etario = age_grp, asfr) %>% 
  mutate(asfr = asfr / 1000) %>% 
  group_by(anio, geocodigoFundar) %>% 
  summarise(tgf = 5*sum(asfr)) %>% 
  ungroup() %>% 
  mutate(fuente = "World Population Prospects (UN)")


# TGF DEIS 2005-2023
df_deis_nacimientos_edades <- df_deis %>% 
  dplyr::filter(!grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  group_by(anio, rango_etario_id) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() 


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


df_deis_tgf_arg <- df_deis_nacimientos_edades %>% 
  left_join(df_wpp_fem_pop_edades_arg, join_by(anio, rango_etario_id)) %>% 
  group_by(anio, geocodigoFundar = "ARG") %>% 
  summarise(tgf = 5*sum(nacimientos / pob_fem)) %>% 
  ungroup() %>% 
  mutate(fuente = "Dirección de Estadísticas e Información en Salud (DEIS)")


df_output <- df_deis_tgf_arg %>% 
  bind_rows(df_wpp_tgf) %>% 
  distinct(anio, geocodigoFundar, .keep_all = T) %>% 
  dplyr::filter(anio <= max(df_deis_tgf_arg$anio)) %>% 
  arrange(anio, geocodigoFundar) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, tgf, fuente)


df_plot <- df_output %>% 
  dplyr::filter(geonombreFundar %in% c("Argentina", "Brasil", "Chile", "Perú", "España", "Uruguay"))


ggplot(df_plot, aes(x=anio, y = tgf, color = geonombreFundar)) + 
  geom_line() + 
  theme_minimal()+
  ylim(0, NA)
