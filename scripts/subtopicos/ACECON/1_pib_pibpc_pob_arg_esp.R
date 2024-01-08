# 1_pib_pibpc_pob_arg_esp
# output con el pib, pib per capita y poblacion de argentina y españa
# a partir de los datos de la web de Fundacion Norte y Sur, World Economic Outlook y Maddison Project Database

subtopico <- "ACECON"
periodo <- 2018:2022

library(tidyverse)


# Insumos -------


# cuentas nacionales fundacion norte y sur 

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))


# IMF outlook database 
# descargo la base entera por mayor facilidad de referencia

download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              destfile = glue::glue("data/{subtopico}/datasets/raw/WEOOct2023all.xls"))



# Maddison database

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx", 
              mode = "wb", # archivos tipo xlsx requieren escritura tipo binaria
              destfile = glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"))

# Lectura y procesamiento -----------

# imf weo
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

weo_imf <- read_tsv(glue::glue("data/{subtopico}/datasets/raw/WEOOct2023all.xls"))

weo_imf <- weo_imf %>% 
  # limpio nombres de columnas: pasar a minusculas, remove non-ascii chars y cambia " " por "_"
  janitor::clean_names()

# maddison database
# GDP pc	Real GDP per capita in 2011$
# Population	Population, mid-year (thousands)


pibpc_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"), sheet = "GDP pc", skip = 1)

pop_maddison_db <- readxl::read_excel(glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"),
                                      sheet = "Population", skip = 1)

# cuentas nacionales fund norte y sur
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004

cn_arg_fnys <- readxl::read_excel(path = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"),
                                  sheet = "PBI en US$", col_names = F) %>% 
  select(1,3,11) %>% .[107:225,] # definicion del analista de datos a usar

# uso los codigos del fmi para renombrar columnas


colnames(cn_arg_fnys) <- c("anio", "ngdp_r", "ngdprpc")

# proceso weo_imf

subset_weo_imf <- weo_imf %>% 
  # selecciono vars de interes
  filter(weo_subject_code %in% c("NGDP_R", "NGDPRPC", "LP")) %>% 
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

subset_weo_imf <- subset_weo_imf %>% rename(poblacion = lp)

# datos char a numericos con limpieza de "," y pasan a unidades simples 
subset_weo_imf <- subset_weo_imf %>% 
  mutate(anio = as.integer(gsub(pattern = "\\D", "",anio)),
         ngdp_r = as.numeric(gsub(",", "", ngdp_r))*1e9,
         ngdprpc = as.numeric(gsub(",", "", ngdprpc)),
         poblacion = as.numeric(gsub(",", "", poblacion))*1e6)

# filtro anios de interes
subset_weo_imf <- subset_weo_imf %>% 
  filter(anio %in% periodo)


subset_weo_imf <- subset_weo_imf %>%
  # completo gdp a partir de gdp per capita y pob
  mutate(ngdp_r = ifelse(is.na(ngdp_r) & !is.na(ngdprpc) & !is.na(poblacion), 
                         ngdprpc*poblacion,
                         ngdp_r
  ),
  # completo gdp per cap a partir de gdp y pob
  ngdprpc = ifelse(is.na(ngdprpc) & !is.na(ngdp_r) & !is.na(poblacion),
                   ngdp_r/poblacion,
                   ngdprpc
  ))

subset_weo_imf <- subset_weo_imf %>%
  # agrupa por pais
  group_by(iso) %>% 
  # excluyo filas con NA en alguna variabl
  filter(if_all(everything(), \(x) !is.na(x))) %>%
  # cuento filas por pais
  mutate(filas = n()) %>%
  # deberia haber tantas filas por pais como anios en estudio
  # paises que no tengan dato para algun anio quedan excluidos
  filter(filas == length(periodo)) %>%
  select(-filas) %>%
  arrange(anio) %>% 
  # calculo las variaciones interanuales para pib, pibpc y pob
  mutate(across(-c(anio),
                \(x) {(x/lag(x))}, .names = "{.col}_var")) %>% 
  ungroup() %>% 
  select(anio, iso, matches("var")) %>%
  filter(anio != 2018) # excluyo el 


#  proceso maddison database pibpc


pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(year %in% 1900:2018)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  # excluyo columnas que solo tienen NA
  select(where(\(x){all(!is.na(x))}))

pibpc_maddison_db <- pibpc_maddison_db %>%
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "ngdprpc")

# proceso maddison db population

pop_maddison_db <- pop_maddison_db %>% 
  filter(year %in% 1900:2018)

pop_maddison_db <- pop_maddison_db %>% 
  # excluyo columnas que solo tienen NA
  select(where(\(x){all(!is.na(x))}))

pop_maddison_db <- pop_maddison_db %>% 
  #  paso a formato largo
  pivot_longer(cols = -year, names_to = "iso", values_to = "poblacion")

subset_maddison_db <- left_join(pibpc_maddison_db, pop_maddison_db, by = c("year", "iso"))

subset_maddison_db <- subset_maddison_db %>% 
  # paso poblacion de miles a unidades simples
  # calculo pib = pibpc*pob
  mutate(poblacion = 1000*poblacion,
         ngdp_r = ngdprpc*poblacion)

subset_maddison_db <- subset_maddison_db %>% 
  filter(iso != "ARG")

subset_maddison_db <- subset_maddison_db %>% 
  rename(anio = year)


# proceso cuentas nacionales fund norte y sur (orlando ferreres)


cn_arg_fnys <- cn_arg_fnys %>% 
  mutate(across(everything(), as.numeric)) %>% 
  # paso pib de miles a unidades simples y calculo pob como pib/pibpc
  mutate(
        ngdp_r = 1000*ngdp_r,
         poblacion = 1000*ngdp_r/ngdprpc, 
         iso = "ARG")

# combina los datasets

# resto del mundo

# al dataset de imf le agrego las filas y columnas del dataset de maddison
pib_pibpc_pob_resto <- bind_rows(subset_weo_imf %>% 
                                   filter(iso != "ARG" &
                                            iso %in% unique(subset_maddison_db$iso)),
                                 subset_maddison_db)

# si no hay duplicaciones deberia dar un df vacio
pib_pibpc_pob_resto%>% group_by(iso) %>% count(anio) %>% filter(n != 1) %>% nrow == 0

# ordeno por pais y anio
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  ungroup() %>% 
  arrange(iso, anio)

# expando la serie maddison usando las variaciones de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  group_by(iso) %>% 
  mutate(ngdp_r = expansor_imf_maddison(ngdp_r, ngdp_r_var),
         ngdprpc = expansor_imf_maddison(ngdprpc, ngdprpc_var),
         poblacion = expansor_imf_maddison(poblacion, poblacion_var)
  ) %>% 
  ungroup()

# calculo los indices base 100 en 1900
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  select(-matches("var")) %>%
  group_by(iso) %>% 
  mutate(pbi_precios_constantes_base1900 = 100*ngdp_r/ngdp_r[anio==1900],
         pbi_per_capita_precios_constantes_base100 = 100*ngdprpc/ngdprpc[anio==1900],
         poblacion_base1900 = 100*poblacion/poblacion[anio==1900]
  ) %>%
  ungroup() %>% 
  select(-c(ngdp_r, ngdprpc, poblacion)) %>%
  rename(iso3 = iso) %>% 
  relocate(iso3, .after = anio) 



# arg

# combino dataset de fund norte y sur con datos imf para argentina
pib_pibpc_pob_arg <- bind_rows(subset_weo_imf %>% 
                                 filter(iso == "ARG"),
                               cn_arg_fnys)

# ordeno por anio
pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  arrange(anio)

# expando la serie de fund norte y sur con las var de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
pib_pibpc_pob_arg$ngdp_r <- expansor_imf_maddison(pib_pibpc_pob_arg$ngdp_r, pib_pibpc_pob_arg$ngdp_r_var)
pib_pibpc_pob_arg$ngdprpc <- expansor_imf_maddison(pib_pibpc_pob_arg$ngdprpc, pib_pibpc_pob_arg$ngdprpc_var)
pib_pibpc_pob_arg$poblacion <- expansor_imf_maddison(pib_pibpc_pob_arg$poblacion, pib_pibpc_pob_arg$poblacion_var)

# calculo los indices base 100 en 1900
pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  select(-matches("var")) %>%
  mutate(pbi_precios_constantes_base1900 = 100*ngdp_r/ngdp_r[anio==1900],
         pbi_per_capita_precios_constantes_base100 = 100*ngdprpc/ngdprpc[anio==1900],
         poblacion_base1900 = 100*poblacion/poblacion[anio==1900]
  ) %>%
  select(-c(ngdp_r, ngdprpc, poblacion)) %>%
  rename(iso3 = iso) %>% 
  relocate(iso3, .after = anio) 


# reuno los datos de arg con los datos del resto del mundo
pib_pibpc_pob <- bind_rows(pib_pibpc_pob_arg, pib_pibpc_pob_resto)


# comparo contra output previo

# descargo outout primera entrega del drive
temp <- tempfile(fileext = ".csv")
url_out_prev <- "https://drive.google.com/file/d/1J0YGNiWdmTpZrH0FlMZef0Kb1_CUeib5/view?usp=sharing"
googledrive::drive_download(url_out_prev, path = temp, overwrite = T)
out_prev <- read.csv2(file = temp)

out_prev <- out_prev %>% 
  mutate(across(-c(pais, iso3), as.numeric))

vs <- out_prev %>% 
  left_join(pib_pibpc_pob, by = c("anio", "iso3"))

diff <-  vs %>% 
  mutate(across(where(is.numeric), \(x) round(x, 2))) %>% 
  filter(pbi_precios_constantes_base1900.x !=  pbi_precios_constantes_base1900.y |
           pbi_per_capita_precios_constantes_base100.x != pbi_per_capita_precios_constantes_base100.y |
           poblacion_base1900.x != poblacion_base1900.y
           ) 


# write output ------

pib_pibpc_pob %>% 
  write_argendata(file_name = "1_pib_pibpc_pob_arg_esp.csv",subtopico =  subtopico)

