# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tasa_global_fecundidad_provincias.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R437C282' # DEIS Nacidos Vivos (2005-2023)
fuente2 <- 'R439C284' # INDEC Población por sexo y grupos quinquenales de edad para el total del país y provincias. Años 2010-2040

df_deis <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)

df_indec <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.)

df_nac_edades <- df_deis %>% 
  dplyr::filter(!(provres %in% c(98,99)), 
                !grepl("Sin especificar", imedad)) %>% 
  mutate(rango_etario_id = as.integer(str_extract(imedad, "(.*)\\.(.*)", group=1))) %>% 
  group_by(provres, prov_desc, anio, rango_etario = imedad, rango_etario_id) %>% 
  summarise(nacimientos = sum(cuenta, na.rm=T)) %>% 
  ungroup() %>% 
  select(provincia_id = provres, anio, rango_etario_id, nacimientos)
  

df_pob_fem_edades <- df_indec %>% 
  dplyr::filter(sexo == "Mujeres") %>%
  mutate(
    edad_ultima = ifelse(edad == "100 y más", 100, as.numeric(str_extract(edad, "(?<=-)[0-9]+")))) %>% 
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
  group_by(anio, provincia_id, provincia, rango_etario_id) %>% 
  summarise(pob_fem = sum(poblacion_proyectada)) %>% 
  ungroup()


df_output <- df_pob_fem_edades %>% 
  inner_join(df_nac_edades, join_by(provincia_id, anio, rango_etario_id)) %>% 
  group_by(anio, provincia_id, provincia) %>% 
  summarise(
    tgf = 5*sum(nacimientos/pob_fem)
  ) %>% 
  ungroup()
  

ggplot(df_output, aes(x=anio, y = tgf, color = provincia)) + 
  geom_line() + 
  theme_minimal()+
  ylim(0, NA)
