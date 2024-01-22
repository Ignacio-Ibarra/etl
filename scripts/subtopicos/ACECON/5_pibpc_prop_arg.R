# 5_pibpc_prop_arg
# descripcion

# vars config del script
output_name <- "5_pibpc_prop_arg"
# periodo, etc.,

# Insumos -------

# maddison db

pibpc_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"),
                                        sheet = "GDP pc", skip = 1)

# imf weo db
weo_imf <- read_tsv(glue::glue("data/{subtopico}/datasets/raw/WEOOct2023all.xls"))

# Procesamiento -------

# datos de world economic outlook fmi


weo_imf <- weo_imf %>% 
  # limpio nombres de columnas: pasar a minusculas, remove non-ascii chars y cambia " " por "_"
  janitor::clean_names()


# proceso weo_imf

subset_weo_imf <- weo_imf %>% 
  # selecciono vars de interes
  filter(weo_subject_code %in% c("NGDPRPPPPC")) %>% 
  select(-c(weo_country_code,country, subject_descriptor, subject_notes, units,
            scale, country_series_specific_notes, estimates_start_after))

# le doy formato longer adecuado
subset_weo_imf <- subset_weo_imf %>% 
  pivot_longer(cols = -c(iso, weo_subject_code), names_to = "anio")

# una columna por indicador
subset_weo_imf <- subset_weo_imf %>% 
  pivot_wider(names_from = weo_subject_code, values_from = value) 

# limpio nombres de columnas (nombre de indicadores)
subset_weo_imf <- janitor::clean_names(subset_weo_imf )


# datos char a numericos con limpieza de "," y pasan a unidades simples 
subset_weo_imf <- subset_weo_imf %>% 
  mutate(anio = as.integer(gsub(pattern = "\\D", "",anio)),
         ngdprppppc = as.numeric(gsub(",", "", ngdprppppc)))

# filtro anios de interes
subset_weo_imf <- subset_weo_imf %>% 
  filter(anio %in% 2018:2022)



subset_weo_imf <- subset_weo_imf %>%
  # agrupa por pais
  group_by(iso) %>% 
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
  select(anio, iso, matches("var")) %>%
  filter(anio != 2018) # excluyo el 

# datos de maddison

pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(year %in% 1900:2018)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  # excluyo columnas que solo tienen NA
  select(where(\(x){all(!is.na(x))}))

pibpc_maddison_db <- pibpc_maddison_db %>%
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "ngdprppppc")

pibpc_maddison_db <- pibpc_maddison_db %>%
  rename(anio = year)

subset_weo_imf <- subset_weo_imf %>% 
  filter(iso %in% unique(pibpc_maddison_db$iso))

# junto los dataframes

df_output <- bind_rows(pibpc_maddison_db, subset_weo_imf) %>% 
  ungroup()

# ordeno por pais y anio
df_output <- df_output %>% 
  arrange(iso, anio) %>% 
  ungroup()

# expando la serie maddison usando las variaciones de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
df_output <- df_output %>% 
  group_by(iso) %>% 
  mutate(ngdprppppc  = expansor_imf_maddison(ngdprppppc , ngdprppppc_var) ) %>% 
  ungroup()

df_output <- df_output %>%
  group_by(anio) %>% 
  mutate(pbi_per_capita_ppa_porcentaje_argentina = 100*ngdprppppc/ngdprppppc[iso == "ARG"]) %>% 
  ungroup()

df_output <- df_output %>%
  rename(iso3 = iso)

df_output <- df_output %>%
  left_join(get_iso_paises())

df_output <- df_output %>%
  select(anio, pais, iso3, pbi_per_capita_ppa_porcentaje_argentina)

# Control vs output previo -------

# descargo outout primera entrega del drive
# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

out_prev <- out_prev %>% 
  mutate(across(c(pbi_per_capita_ppa_porcentaje_argentina), as.numeric))

vs <- out_prev %>% 
  left_join(df_output, by = c("anio", "iso3", "pais"))

diff <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  filter(pbi_per_capita_ppa_porcentaje_argentina.x !=  pbi_per_capita_ppa_porcentaje_argentina.y ) 

diff %>% 
  write_argendata(file_name = glue::glue("_diff_{output_name}.csv"),
                  subtopico =  subtopico)

# Write output ------


df_output %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
                  subtopico = subtopico)
