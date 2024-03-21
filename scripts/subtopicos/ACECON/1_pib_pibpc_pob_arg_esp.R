# dataset: 1_pib_pibpc_pob_arg_esp.csv
# output con el pib, pib per capita y poblacion de argentina y españa


# lectura de datos --------------------------------------------------------


# maddison database

maddison_db <- read_csv("data/_FUENTES/clean/mpd2020.csv")

# cuentas nacionales fund norte y sur
# PIB moneda nacional constante 2004 (esta en miles)
# PIB per capita moneda nacional constante 2004
cuentas_nacionales <- read_csv("data/_FUENTES/clean/cuentas-nacionales-fundacion-norte-y-sur.csv")

# imf weo
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)

weo_imf <- read_csv("data/_FUENTES/clean/weo_imf.csv")
diccionario_weo <- read_csv("data/_FUENTES/clean/diccionario_weo_imf.csv")

# parametros generales ---------

periodo_weo <- 2018:2022
output_name <- "1_pib_pibpc_pob_arg_esp"


# procesamiento -----------


# proceso weo_imf

weo_imf <- weo_imf %>% 
  # selecciono vars de interes
  filter(weo_subject_code %in% c("NGDP_R", "NGDPRPC", "LP")) 



# una columna por indicador
weo_imf <- weo_imf %>% 
  pivot_wider(names_from = weo_subject_code, values_from = valor) 

# limpio nombres de columnas (nombre de indicadores)
weo_imf <- janitor::clean_names(weo_imf )

weo_imf <- weo_imf %>% rename(poblacion = lp)


# filtro anios de interes
weo_imf <- weo_imf %>% 
  filter(anio %in% periodo_weo)


weo_imf <- weo_imf %>%
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

weo_imf <- weo_imf %>%
  # agrupa por pais
  group_by(iso3) %>% 
  # excluyo filas con NA en alguna variabl
  filter(if_all(everything(), function(x) !is.na(x))) %>%
  # cuento filas por pais
  mutate(filas = n()) %>%
  # deberia haber tantas filas por pais como anios en estudio
  # paises que no tengan dato para algun anio quedan excluidos
  filter(filas == length(periodo_weo)) %>%
  select(-filas) %>%
  arrange(anio) %>% 
  # calculo las variaciones interanuales para pib, pibpc y pob
  mutate(across(-c(anio),
                function(x) {(x/lag(x))}, .names = "{.col}_var")) %>% 
  ungroup() %>% 
  select(anio, iso3, matches("var")) %>%
  filter(anio != 2018) # excluyo el 


#  proceso maddison database pibpc

maddison_db <- maddison_db %>% 
  filter(anio %in% 1900:2018 & indicador %in% c("gdppc", "pop"))


maddison_db <- maddison_db %>%
  #  paso a formato largo
  pivot_wider(names_from = indicador, values_from = valor)

maddison_db <- maddison_db %>% 
  rename(ngdprpc = gdppc, poblacion = pop)


maddison_db <- maddison_db %>% 
  mutate(ngdp_r = ngdprpc * poblacion )
  
maddison_db <- maddison_db %>% 
  filter(iso3 != "ARG")


# proceso cuentas nacionales fund norte y sur (orlando ferreres)

cn_arg_fnys <- cuentas_nacionales %>% 
  select(1,3,11) %>% .[107:225,] # definicion del analista de datos a usar

# uso los codigos del fmi para renombrar columnas
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004
colnames(cn_arg_fnys) <- c("anio", "ngdp_r", "ngdprpc")

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


# comparo contra output previo -----

# se puede leer outoput del drive directo desde la url
out_prev <- read.csv2(file = glue::glue("https://drive.usercontent.google.com/download?id={outputs$id[grepl(output_name, outputs$name)]}"))

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
diff %>% 
  as_tibble() %>% 
  head(25)

diff %>% 
  write_argendata(file_name = "_diff_1_pib_pibpc_pob_arg_esp.csv",
                  subtopico =  subtopico)

# write output ------
iso_countrycodes <- get_iso_paises()

pib_pibpc_pob %>%
  left_join(iso_countrycodes) %>% 
  relocate(pais, .after = iso3) %>% 
  write_argendata(file_name = glue::glue("{output_name}.csv"),
                  subtopico = subtopico)

rm(list = ls())

