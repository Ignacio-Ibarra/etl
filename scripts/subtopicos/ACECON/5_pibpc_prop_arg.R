# 5_pibpc_prop_arg
# descripcion

# vars config del script
output_name <- "5_pibpc_prop_arg"
# periodo, etc.,

# Insumos -------

# maddison db

pibpc_maddison_db <- read_csv(fuentes_files[grepl("R37C1", fuentes_files)]) 


# imf weo db
weo_imf <- read_csv(fuentes_files[grepl("R34C2", fuentes_files)])

# Procesamiento -------

# datos de world economic outlook fmi

# proceso weo_imf

subset_weo_imf <- weo_imf %>% 
  # selecciono vars de interes
  filter(weo_subject_code %in% c("NGDPRPPPPC"))

# filtro anios de interes
subset_weo_imf <- subset_weo_imf %>% 
  filter(anio %in% 2018:2022)

subset_weo_imf <- subset_weo_imf %>% 
  pivot_wider(names_from = weo_subject_code, values_from = valor)

subset_weo_imf <- subset_weo_imf %>% 
  janitor::clean_names()

subset_weo_imf <- subset_weo_imf %>%
  # agrupa por pais
  group_by(iso3) %>% 
  # excluyo filas con NA en alguna variabl
  filter(if_all(everything(), function(x) !is.na(x))) %>%
  # cuento filas por pais
  mutate(filas = n()) %>%
  # deberia haber tantas filas por pais como anios en estudio
  # paises que no tengan dato para algun anio quedan excluidos
  filter(filas == length(2018:2022)) %>%
  select(-filas) %>%
  arrange(anio) %>% 
  # calculo las variaciones interanuales para pib pc constante a ppp
  mutate(ngdprppppc_var = ngdprppppc/lag(ngdprppppc)) %>% 
  ungroup() %>% 
  select(anio, iso3, matches("var")) %>%
  filter(anio != 2018) # excluyo el 

# datos de maddison

pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(anio %in% 1900:2018 & indicador == "gdppc")


pibpc_maddison_db <- pibpc_maddison_db %>%
  #  paso a formato largo
  pivot_wider(names_from = indicador,  values_from = "valor")



subset_weo_imf <- subset_weo_imf %>% 
  filter(iso3 %in% unique(pibpc_maddison_db$iso3))

# junto los dataframes

df_output <- bind_rows(pibpc_maddison_db, subset_weo_imf) %>% 
  ungroup()



# expando la serie maddison usando las variaciones de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
df_output <- df_output %>% 
  group_by(iso3) %>% 
  arrange(anio) %>% 
  mutate(gdppc  = expansor_xvar(gdppc , ngdprppppc_var) ) %>% 
  ungroup()

df_output <- df_output %>%
  group_by(anio) %>% 
  mutate(pbi_per_capita_ppa_porcentaje_argentina = 100*gdppc/gdppc[iso3 == "ARG"]) %>% 
  ungroup()


df_output %>% 
  select(-country)

all(df_output$iso3 %in% get_nomenclador_geografico()$codigo_fundar)

nrow(df_output)

df_output <- df_output %>%
  left_join(get_nomenclador_geografico() %>% 
              select(iso3 = codigo_fundar, pais = desc_fundar))

nrow(df_output)


df_output <- df_output %>%
  select(anio, pais, iso3, pbi_per_capita_ppa_porcentaje_argentina)

df_output <- df_output %>%
  complete(anio = 1900:2022, iso3)

paises_serie_incompleta <- df_output %>% 
  filter(is.na(pbi_per_capita_ppa_porcentaje_argentina)) %>% 
  pull(iso3) %>% unique()

df_output <- df_output %>% 
  filter(! iso3 %in% paises_serie_incompleta)

# Control vs output previo -------

comparacion <- comparar_outputs(df_output %>% 
                                  mutate(pais = replace_non_ascii(pais)), nombre = output_name,
                                subtopico = subtopico, pk = c("anio", "iso3"), drop_output_drive = F)

# Write output ------


df_output %>% 
  write_output(output_name = output_name, subtopico = subtopico,
               fuentes = c("R37C1", "R34C2"), analista = analista, pk = c("anio", "iso3"),
               es_serie_tiempo = T,columna_indice_tiempo = "anio", columna_geo_referencia = "iso3", nivel_agregacion = "pais",
               etiquetas_indicadores = list("pbi_per_capita_ppa_porcentaje_argentina" = "PBI per cápita PPA como porcentaje del de Argentina"),
               unidades = list("pbi_per_capita_ppa_porcentaje_argentina" = "porcentaje"))
