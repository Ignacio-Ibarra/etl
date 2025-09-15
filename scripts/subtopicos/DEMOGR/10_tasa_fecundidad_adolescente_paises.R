# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_fecundidad_adolescente_paises.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R438C283' # World Population Proscpects - Fertility. 1950-2100, 5-year age groups. Age-specific Fertility Rate (ASFR). Percent Age-specific Fertility Rate (PASFR). Births (thousands). CSV format
fuente2 <- 'R435C280' # World Population Proscpects - Population
fuente3 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)

df_wpp_fertility <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_wpp_pob <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_deis <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.)

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_wpp_nacimientos_adolescente <-df_wpp_fertility %>% 
  dplyr::filter(!is.na(iso3_code),
                trimws(iso3_code) != '',
                variant == "Medium",
                age_grp %in% c("10-14","15-19")) %>% 
  group_by(anio = time, geocodigoFundar = iso3_code) %>% 
  summarise(nacimientos_adolescentes = sum(births, na.rm =T)*1000) %>% 
  left_join(geo_front, join_by(geocodigoFundar))%>% 
  dplyr::filter(anio <= (year(Sys.Date())))

rm(df_wpp_fertility)


df_pop_fem_adolescente <- df_wpp_pob %>% 
  dplyr::filter(!is.na(iso3_code),
                trimws(iso3_code) != '',
                variant == "Medium",
                age_grp %in% c("10-14","15-19")) %>% 
  group_by(anio = time, geocodigoFundar = iso3_code) %>% 
  summarise(pob_fem_adolescente = sum(pop_female, na.rm =T)*1000) 

rm(df_wpp_pob)



df_deis_nacimientos_edades_adolescentes_arg <- df_deis %>% 
  dplyr::filter(!grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  dplyr::filter(rango_etario_id %in% 1:2) %>% 
  group_by(anio) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() 

df_deis_tgf_adolescente <- df_deis_nacimientos_edades_adolescentes_arg %>% 
  left_join(df_pop_fem_adolescente %>% 
              dplyr::filter(geocodigoFundar == "ARG"),
            join_by(anio)) %>% 
  mutate(tgf = 1000 * nacimientos / pob_fem_adolescente, 
         fuente = "Dirección de Estadísticas e Información en Salud (DEIS)") %>% 
  select(anio, geocodigoFundar, tgf, fuente)


df_wpp_tgf_adolescente <- df_wpp_nacimientos_adolescente %>% 
  left_join(df_pop_fem_adolescente, join_by(anio, geocodigoFundar)) %>% 
  mutate(
    tgf = 1000* nacimientos_adolescentes / pob_fem_adolescente,
    fuente =  "World Population Prospects (UN)"
  ) %>% 
  select(anio, geocodigoFundar, tgf, fuente)


df_output <- df_deis_tgf_adolescente %>% 
  bind_rows(df_wpp_tgf_adolescente) %>% 
  distinct(anio, geocodigoFundar, .keep_all = T) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  select(anio, geocodigoFundar, geonombreFundar, tgf_adolescente = tgf, fuente) %>% 
  arrange(anio, geocodigoFundar) %>% 
  dplyr::filter(anio <= max(df_deis_tgf_adolescente$anio))


df_plot <- df_output %>% 
  dplyr::filter(geonombreFundar %in% c("Argentina", "Brasil", "Chile", "Perú", "España", "Uruguay"))

ggplot(df_plot, aes(x=anio, y = tgf, color = geonombreFundar)) + 
  geom_line() + 
  theme_minimal()+
  ylim(0, NA)
