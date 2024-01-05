# 1_pib_pibpc_pob_arg_esp
# output con el pib, pib per capita y poblacion de argentina y españa
# a partir de los datos de la web de Fundación Norte y Sur, World Economic Outlook y Maddison Project Database

library(tidyverse)

expansor <- function(x,y) {
  for (i in 1:length(x)) {
    if (is.na(x[i])) {
      x[i] <- x[i-1]*y[i]
    } 
  }
  x
}


# Insumos -------


# cuentas nacionales fundacion norte y sur 

download.file("https://docs.google.com/spreadsheets/d/e/2PACX-1vTAGGfIqDw18YDI5zasGBRa4sG1ddUfMcKT87fzTkvz8HMe8Ipl6zJU0M2788oZrw/pub?output=xls",
              mode = "wb",
              destfile = glue::glue("data/{subtopico}/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx"))


# IMF outlook database 
# descargo la base entera por mayor facilidad de referencia

download.file(url = "https://www.imf.org/-/media/Files/Publications/WEO/WEO-Database/2023/WEOOct2023all.ashx",
              destfile = glue::glue("data/{subtopico}/datasets/raw/WEOOct2023all.xls"))



# Maddison database

download.file(url = "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.xlsx", 
              mode = "wb",
              destfile = glue::glue("data/{subtopico}/datasets/raw/mpd2020.xlsx"))

# Lectura y procesamiento -----------

# imf weo
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

weo_imf <- read_tsv("data/ACECON/datasets/raw/WEOOct2023all.xls")

weo_imf <- weo_imf %>% 
  janitor::clean_names()

# TODOS

subset_weo_imf <- weo_imf %>% 
  filter(weo_subject_code %in% c("NGDP_R", "NGDPRPC", "LP")) %>% 
  select(-c(weo_country_code,country, subject_descriptor, subject_notes, units, scale, country_series_specific_notes, estimates_start_after))


subset_weo_imf <- subset_weo_imf %>% 
  pivot_longer(cols = -c(iso, weo_subject_code), names_to = "anio")

subset_weo_imf <- subset_weo_imf %>% 
  pivot_wider(names_from = weo_subject_code, values_from = value) 

subset_weo_imf <- janitor::clean_names(subset_weo_imf )

subset_weo_imf <- subset_weo_imf %>% rename(poblacion = lp)


subset_weo_imf <- subset_weo_imf %>% 
  mutate(anio = as.integer(gsub(pattern = "\\D", "",anio)),
         ngdp_r = as.numeric(ngdp_r)*1e9,
         ngdprpc = as.numeric(gsub(",", "", ngdprpc)),
         poblacion = as.numeric(poblacion)*1e6)

subset_weo_imf <- subset_weo_imf %>% 
  filter(anio %in% 2018:2022)

subset_weo_imf <- subset_weo_imf %>% 
  mutate(ngdp_r = ifelse(is.na(ngdp_r) & !is.na(ngdprpc) & !is.na(poblacion), 
                         ngdprpc*poblacion,
                         ngdp_r
                         ),
         ngdprpc = ifelse(is.na(ngdprpc) & !is.na(ngdp_r) & !is.na(poblacion),
                          ngdp_r/poblacion,
                          ngdprpc
                          ))

subset_weo_imf <- subset_weo_imf %>% 
  group_by(iso) %>% 
  filter(if_all(everything(), \(x) !is.na(x))) %>% 
  mutate(filas = n()) %>% 
  filter(filas == length(2018:2022)) %>%
  select(-filas) %>% 
  arrange(anio) %>% 
  mutate(across(-c(anio), \(x) {(x/lag(x))}, .names = "{.col}_var")) %>% 
  ungroup() %>% 
  select(anio, iso, matches("var")) %>% 
  filter(anio != 2018)


# ARG
# arg_weo_imf <- weo_imf %>% 
#   filter(iso == "ARG" & weo_subject_code %in% c("NGDP_R", "NGDPRPC", "LP")) %>% 
#   select(-c(iso, weo_country_code,country, subject_descriptor, subject_notes, units, scale, country_series_specific_notes, estimates_start_after))
# 
# arg_weo_imf_metadata <- weo_imf %>% 
#   filter(iso == "ARG" & weo_subject_code %in% c("NGDP_R", "NGDPRPC", "LP")) %>% 
#   distinct(weo_country_code,country, subject_descriptor, subject_notes, units, scale, country_series_specific_notes, estimates_start_after)
# 
# 
# arg_weo_imf <- arg_weo_imf %>% 
#   pivot_longer(cols = -c(weo_subject_code), names_to = "anio")
# 
# arg_weo_imf <- arg_weo_imf %>% 
#   pivot_wider(names_from = weo_subject_code, values_from = value) 
# 
# arg_weo_imf <- janitor::clean_names(arg_weo_imf )
# 
# arg_weo_imf <- arg_weo_imf %>% rename(poblacion = lp)
# 
# 
# arg_weo_imf <- arg_weo_imf %>% 
#   mutate(anio = as.integer(gsub(pattern = "\\D", "",anio)),
#          ngdp_r = as.numeric(ngdp_r)*1e9,
#          ngdprpc = as.numeric(gsub(",", "", ngdprpc)),
#          poblacion = as.numeric(poblacion)*1e6)
# 
# arg_weo_imf <- arg_weo_imf %>% 
#   filter(anio %in% 2018:2022)

# combinar datos de arg_weo_imf y cn_arg_fnys
# 15. Utilizar la fórmula para calcular las tasas de crecimiento por año: x/lag(x)
# se usan las tasas de los datos del imf para expandir la serie de fund norte y sur

# arg_weo_imf <- arg_weo_imf %>% 
#   mutate(across(-c(anio), \(x) {(x/lag(x))}, .names = "{.col}_var")) %>% 
#   select(anio, matches("var"))

# maddison database
# GDP pc	Real GDP per capita in 2011$
# Population	Population, mid-year (thousands)


readxl::excel_sheets("data/ACECON/datasets/raw/mpd2020.xlsx")

pibpc_maddison_db <- readxl::read_excel("data/ACECON/datasets/raw/mpd2020.xlsx", sheet = "GDP pc", skip = 1)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  filter(year %in% 1900:2018)

pibpc_maddison_db <- pibpc_maddison_db %>% 
  select(where(\(x){all(!is.na(x))}))

pibpc_maddison_db <- pibpc_maddison_db %>% 
  pivot_longer(cols = -year, names_to = "iso", values_to = "ngdprpc")

pop_maddison_db <- readxl::read_excel("data/ACECON/datasets/raw/mpd2020.xlsx",
                                        sheet = "Population", skip = 1)

pop_maddison_db <- pop_maddison_db %>% 
  filter(year %in% 1900:2018)

pop_maddison_db <- pop_maddison_db %>% 
  select(where(\(x){all(!is.na(x))}))

pop_maddison_db <- pop_maddison_db %>% 
  pivot_longer(cols = -year, names_to = "iso", values_to = "poblacion")

subset_maddison_db <- left_join(pibpc_maddison_db, pop_maddison_db, by = c("year", "iso"))

subset_maddison_db <- subset_maddison_db %>% 
  mutate(poblacion = 1000*poblacion,
         ngdp_r = ngdprpc*poblacion)

subset_maddison_db <- subset_maddison_db %>% 
  filter(iso != "ARG")

subset_maddison_db <- subset_maddison_db %>% 
  rename(anio = year)

# cuentas nacionales fund norte y sur

readxl::excel_sheets("data/ACECON/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx")

cn_arg_fnys <- readxl::read_excel(path = "data/ACECON/datasets/raw/cuentas-nacionales-fund-norte-y-sur.xlsx",
                   sheet = "PBI en US$", col_names = F) %>% 
  select(1,3,11) %>% .[107:225,] 

# uso los codigos del fmi para nombrar columnas
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004
colnames(cn_arg_fnys) <- c("anio", "ngdp_r", "ngdprpc")

cn_arg_fnys <- cn_arg_fnys %>% 
  mutate(across(everything(), as.numeric)) %>% 
  mutate(poblacion = ngdp_r*1000/ngdprpc, 
         iso = "ARG")

# combina los datasets

# resto del mundo


pib_pibpc_pob_resto <- full_join(subset_weo_imf %>% 
                                 filter(iso != "ARG" &
                                          iso %in% unique(subset_maddison_db$iso)),
                               subset_maddison_db,
                               by = c("anio", "iso"))

pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  ungroup() %>% 
  arrange(iso, anio)

pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  group_by(iso) %>% 
  mutate(ngdp_r = expansor(ngdp_r, ngdp_r_var),
         ngdprpc = expansor(ngdprpc, ngdprpc_var),
         poblacion = expansor(poblacion, poblacion_var)
         ) %>% 
  ungroup()


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

pib_pibpc_pob_arg <- full_join(subset_weo_imf %>% 
                                 filter(iso == "ARG"),
                               cn_arg_fnys,
                               by = c("anio", "iso"))

pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  arrange(anio)

pib_pibpc_pob_arg$ngdp_r <- expansor(pib_pibpc_pob_arg$ngdp_r, pib_pibpc_pob_arg$ngdp_r_var)
pib_pibpc_pob_arg$ngdprpc <- expansor(pib_pibpc_pob_arg$ngdprpc, pib_pibpc_pob_arg$ngdprpc_var)
pib_pibpc_pob_arg$poblacion <- expansor(pib_pibpc_pob_arg$poblacion, pib_pibpc_pob_arg$poblacion_var)

pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  select(-matches("var")) %>% 
  mutate(pbi_precios_constantes_base1900 = 100*ngdp_r/ngdp_r[anio==1900],
         pbi_per_capita_precios_constantes_base100 = 100*ngdprpc/ngdprpc[anio==1900],
         poblacion_base1900 = 100*poblacion/poblacion[anio==1900]
         ) %>%
  select(-c(ngdp_r, ngdprpc, poblacion)) %>%
  rename(iso3 = iso) %>% 
  relocate(iso3, .after = anio) 

pib_pibpc_pob <- bind_rows(pib_pibpc_pob_arg, pib_pibpc_pob_resto)

pib_pibpc_pob %>% 

