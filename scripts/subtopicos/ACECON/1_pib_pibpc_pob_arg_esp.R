# dataset: 1_pib_pibpc_pob_arg_esp.csv
# output con el pib, pib per capita y poblacion de argentina y españa


# lectura de datos --------------------------------------------------------


# maddison database
# R37C1
maddison_db <- read_csv(fuentes_files[grepl("R37C1", fuentes_files)])

# cuentas nacionales fund norte y sur
# PIB moneda nacional constante 2004 (esta en miles)
# PIB per capita moneda nacional constante 2004
# R36C9
pbi_fnys <-  read_csv(fuentes_files[grepl("R36C9", fuentes_files)])

# imf weo
# unidades 
# "NGDP_R" (pib) esta en miles de millones de moneda nacional constantes (1e9)
# "NGDPRPC" (pib per capita) esta en moneda nacional constantes
# "LP" (poblacion) esta en millones (1e6)
# R34C2
weo_imf <- read_csv(fuentes_files[grepl("R34C2", fuentes_files)])

# diccionario weo imf R34C3
diccionario_weo <- read_csv(fuentes_files[grepl("R34C3", fuentes_files)])

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
  filter(anio != 2018) 


#  proceso maddison database pibpc

maddison_db <- maddison_db %>% 
  complete(anio, iso3, indicador)

paises_excluidos <- maddison_db %>% 
  filter(is.na(valor) & anio == 1900) %>% 
  pull(iso3) %>% unique()

maddison_db <- maddison_db %>% 
  filter(anio %in% 1900:2018 & indicador %in% c("gdppc", "pop") & 
           ! iso3 %in% paises_excluidos)


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

pbi_fnys <- pbi_fnys %>% 
  filter((indicador == "PIB a precios de mercado" &
           unidad == "miles de $ de 2004") |
           (indicador == "PIB per capita a precios de mercado" &
              unidad == "$ de 2004 / hab")) 
  
pbi_fnys <- pbi_fnys %>% 
  pivot_wider(id_cols = c(anio, iso3), names_from = indicador, 
              values_from = valor) %>% 
  janitor::clean_names()

pbi_fnys <- pbi_fnys %>% 
  mutate(pib_a_precios_de_mercado = 1000*pib_a_precios_de_mercado,
         poblacion = pib_a_precios_de_mercado/pib_per_capita_a_precios_de_mercado)


# uso los codigos del fmi para renombrar columnas
# ngdp_r = PIB moneda nacional constante 2004 (esta en miles)
# ngdprpc = PIB per capita moneda nacional constante 2004

pbi_fnys <- pbi_fnys %>% 
  rename(ngdp_r = pib_a_precios_de_mercado,
         ngdprpc = pib_per_capita_a_precios_de_mercado)


# combina los datasets

# resto del mundo

# al dataset de imf le agrego las filas y columnas del dataset de maddison
pib_pibpc_pob_resto <- bind_rows(weo_imf %>% 
                                   filter(iso3 != "ARG" &
                                            iso3 %in% unique(maddison_db$iso3)),
                                 maddison_db)

# si no hay duplicaciones deberia dar un df vacio
pib_pibpc_pob_resto%>% group_by(iso3) %>%
  count(anio) %>% filter(n != 1) %>% nrow == 0

# ordeno por pais y anio
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  ungroup() %>% 
  arrange(iso3, anio)

# expando la serie maddison usando las variaciones de imf
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  group_by(iso3) %>% 
  mutate(ngdp_r = expansor_xvar(ngdp_r, ngdp_r_var),
         ngdprpc = expansor_xvar(ngdprpc, ngdprpc_var),
         poblacion = expansor_xvar(poblacion, poblacion_var)
  ) %>% 
  ungroup()

# calculo los indices base 100 en 1900
pib_pibpc_pob_resto <- pib_pibpc_pob_resto %>% 
  select(-matches("var")) %>%
  group_by(iso3) %>% 
  mutate(pbi_precios_constantes_base1900 = 100*ngdp_r/ngdp_r[anio==1900],
         pbi_per_capita_precios_constantes_base1900 = 100*ngdprpc/ngdprpc[anio==1900],
         poblacion_base1900 = 100*poblacion/poblacion[anio==1900]
  ) %>%
  ungroup() %>% 
  select(-c(ngdp_r, ngdprpc, poblacion)) %>%
  relocate(iso3, .after = anio) 



# arg

# combino dataset de fund norte y sur con datos imf para argentina
pib_pibpc_pob_arg <- bind_rows(weo_imf %>% 
                                 filter(iso3 == "ARG"),
                               pbi_fnys)

# ordeno por anio
pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  arrange(anio)

# expando la serie de fund norte y sur con las var de imf
# ver aclaracion de la funcion en scripts/aux_functions.R
pib_pibpc_pob_arg$ngdp_r <- expansor_xvar(pib_pibpc_pob_arg$ngdp_r, pib_pibpc_pob_arg$ngdp_r_var)
pib_pibpc_pob_arg$ngdprpc <- expansor_xvar(pib_pibpc_pob_arg$ngdprpc, pib_pibpc_pob_arg$ngdprpc_var)
pib_pibpc_pob_arg$poblacion <- expansor_xvar(pib_pibpc_pob_arg$poblacion, pib_pibpc_pob_arg$poblacion_var)

# calculo los indices base 100 en 1900
pib_pibpc_pob_arg <- pib_pibpc_pob_arg %>% 
  select(-matches("var")) %>%
  mutate(pbi_precios_constantes_base1900 = 100*ngdp_r/ngdp_r[anio==1900],
         pbi_per_capita_precios_constantes_base1900 = 100*ngdprpc/ngdprpc[anio==1900],
         poblacion_base1900 = 100*poblacion/poblacion[anio==1900]
  ) %>%
  select(-c(ngdp_r, ngdprpc, poblacion))


# reuno los datos de arg con los datos del resto del mundo
output <- bind_rows(pib_pibpc_pob_arg, 
                           pib_pibpc_pob_resto) %>% 
  select(- country)

# etiquetas de pais 

unique(output$iso3[! output$iso3 %in% argendataR::get_nomenclador_geografico()$codigo_fundar])

nrow(output)

output <- left_join(output, argendataR::get_nomenclador_geografico() %>% 
                      select(codigo_fundar, desc_fundar),
                    by = c("iso3" = "codigo_fundar") )

output <- output %>% 
  rename(pais = desc_fundar)

output$pais %>% is.na() %>% sum()

nrow(output)

# excluyo paises con datos faltantes en la serie de maddison

incompletos <- output %>% complete(anio = 1900:2018, iso3) %>%
  group_by(iso3) %>%
  filter(if_any(everything(), is.na)) %>% 
  pull(iso3) %>% unique()

output <- output %>% 
  filter(! iso3 %in% incompletos)


# comparo contra output previo -----

comparacion <- comparar_outputs(df = output %>% 
                                  mutate(pais = replace_non_ascii(pais)),
                                nombre = output_name,
                                subtopico = subtopico, pk = c("anio", "iso3"),
                                drop_output_drive = F)

# write output ------



  
output %>% 
  write_output(data = .,
              output_name = output_name,
             subtopico = subtopico,
             fuentes = c("R34C3", "R34C2","R37C1", "R36C9"),
             analista = analista,
             exportar = T,
             pk = c("iso3", "anio"),
             columna_indice_tiempo = "anio",
             es_serie_tiempo = T,
             etiquetas_indicadores = list(anio = "Año", 
                                          "iso3" = "País Código ISO3",
                                          "pbi_precios_constantes_base1900" = "Producto Bruto Interno, Precios Constantes (1900=100)",
                                          "pbi_per_capita_precios_constantes_base1900" = "Producto Bruto Interno per cápita, Precios Constantes (1900=100)",
                                          "poblacion_base1900" = "Población (1900=100)"),
             unidades = list("anio" = "años", 
                             "pbi_precios_constantes_base1900" = "índice",
                             "pbi_per_capita_precios_constantes_base1900" = "índice",
                             "poblacion_base1900" = "índice"),
             columna_geo_referencia = "iso3",
             nivel_agregacion = "pais")

rm(list = ls())


