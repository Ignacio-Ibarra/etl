# 4_pibpc_prop_eeuu
# descripcion

# vars config del script
output_name <- "4_pibpc_prop_eeuu"
# periodo, etc.,

# Insumos -------

# maddison db R37C1
# gpdpc 
pibpc_maddison_db <- read_csv(get_temp_path("R37C1"))

# imf weo db
weo_imf <- read_csv(get_temp_path("R34C2"))

# Procesamiento -------

# datos de world economic outlook fmi


# proceso weo_imf

subset_weo_imf <- weo_imf %>% 
  # selecciono vars de interes
  filter(weo_subject_code %in% c("NGDPRPPPPC")) 

# una columna por indicador
subset_weo_imf <- subset_weo_imf %>% 
  pivot_wider(names_from = weo_subject_code, values_from = valor) 

# limpio nombres de columnas (nombre de indicadores)
subset_weo_imf <- janitor::clean_names(subset_weo_imf )


# filtro anios de interes
subset_weo_imf <- subset_weo_imf %>% 
  filter(anio %in% 2018:2022)



subset_weo_imf <- subset_weo_imf %>%
  # agrupa por pais
  group_by(iso3) %>% 
  # excluyo filas con NA en alguna variabl
  filter(if_all(everything(), \(x) !is.na(x))) %>%
  # cuento filas por pais
  mutate(filas = n()) %>%
  # deberia haber tantas filas por pais como anios en estudio
  # paises que no tengan dato para algun anio quedan excluidos
  filter(filas == length(2018:2022)) %>%
  select(-filas) %>%
  arrange(anio) %>% 
  # calculo las variaciones interanuales para pib pc constante a ppp
  mutate(across(-c(anio),
                \(x) {(x/lag(x))}, .names = "{.col}_var")) %>% 
  ungroup() %>% 
  select(anio, iso3, matches("var")) %>%
  filter(anio != 2018) # excluyo el 

# datos de maddison

pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(anio %in% 1900:2018 & indicador == "gdppc")

pibpc_maddison_db <- pibpc_maddison_db %>% 
  rename(ngdprppppc = valor)

subset_weo_imf <- subset_weo_imf %>% 
  filter(iso3 %in% unique(pibpc_maddison_db$iso3))

 
# junto los dataframes

df_output <- bind_rows(pibpc_maddison_db, subset_weo_imf) %>% 
  ungroup()

# ordeno por pais y anio
df_output <- df_output %>% 
  arrange(iso3, anio) %>% 
  ungroup()

# expando la serie maddison usando las variaciones de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
df_output <- df_output %>% 
  group_by(iso3) %>% 
  mutate(ngdprppppc  = expansor_xvar(ngdprppppc , ngdprppppc_var) ) %>% 
  ungroup()

df_output <- df_output %>%
  group_by(anio) %>% 
  mutate(pib_per_capita_ppa_porcentaje_eeuu = 100*ngdprppppc/ngdprppppc[iso3 == "USA"]) %>% 
  ungroup()

df_output <- df_output %>% 
  select(-c(country, indicador, ngdprppppc_var, ngdprppppc))

df_output <- df_output %>% 
  complete(anio = 1900:2022, iso3)

incompletos <- df_output %>% 
  filter(is.na(pib_per_capita_ppa_porcentaje_eeuu)) %>% 
  pull(iso3) %>% unique()
  
df_output <- df_output %>% 
  filter(!iso3 %in% incompletos)


unique(df_output$iso3 [! df_output$iso3 %in% get_nomenclador_geografico()[["codigo_fundar"]]])

nrow(df_output)

df_output <-left_join(df_output, get_nomenclador_geografico() %>% 
                        select(codigo_fundar, pais = desc_fundar), by = c("iso3" = "codigo_fundar"))
nrow(df_output)

df_output <- df_output %>%
  select(anio, pais, iso3, pib_per_capita_ppa_porcentaje_eeuu)

# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url

comparacion <- comparar_outputs(df = df_output %>% 
                                  mutate(pais = replace_non_ascii(pais)),
                                nombre = output_name,
                                pk = c("iso3", "anio"), drop_output_drive = F)

# Write output ------

write_output(
  data = df_output,
  output_name = output_name,subtopico = subtopico,
  fuentes = c("R37C1", "R34C2"),
  analista = analista,
  pk = c("anio", "iso3"),
  es_serie_tiempo = T,
  columna_indice_tiempo = "anio",
  columna_geo_referencia = "iso3",
  nivel_agregacion = "pais",
  etiquetas_indicadores = list("pib_per_capita_ppa_porcentaje_eeuu" = "PBI per capita PPA como porcentaje respecto de EE.UU."),
  unidades = list("pib_per_capita_ppa_porcentaje_eeuu" = "Porcentaje")
)
