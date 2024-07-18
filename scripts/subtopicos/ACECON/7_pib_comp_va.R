# 7_pib_comp_va
# descripcion

# vars config del script
output_name <- "7_pib_comp_va"
subtopico <- "ACECON"
# periodo, etc.,

# Insumos -------

# sectores a precios basicos como % del pib a precio de mercado y PIB a precio de basicos como % respecto a precio de mercado
pbisectores_fnys <- read_csv(get_temp_path("R36C13"))

# pib en usd “PIB a precios de mercado, en miles de $ 2018
pbiusd_fnys <- read_csv(get_temp_path("R36C9"))

# vab por sectores a precios corrientes en millones de pesos corrientes 
pbisectores_indec <- read_csv(get_temp_path("R38C7"))

# Procesamiento -------

#2 me quedo solo con los datos de total anual
pbisectores_indec <- pbisectores_indec %>% 
  filter(trim == "Total") %>% 
  select(-trim) 

#3 selecciono anios que corresponden a la expansion
pbisectores_indec <- pbisectores_indec %>%
  filter(anio %in% 2018:2022)

#4 seleccion de variables de interes
pbisectores_indec <- pbisectores_indec %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  select(anio, producto_interno_bruto, a_agricultura_ganaderia_caza_y_silvicultura,
         b_pesca, c_explotacion_de_minas_y_canteras, d_industria_manufacturera,
         e_electricidad_gas_y_agua, f_construccion,
         g_comercio_mayorista_minorista_y_reparaciones,h_hoteles_y_restaurantes, i_transporte_almacenamiento_y_comunicaciones,
         j_intermediacion_financiera, k_actividades_inmobiliarias_empresariales_y_de_alquiler,
         l_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria,
         m_ensenanza, n_servicios_sociales_y_de_salud,
         o_otras_actividades_de_servicios_comunitarias_sociales_y_personales, p_hogares_privados_con_servicio_domestico)



#6 se agrupan subsectores en sectores que coinciden con la serie de fund norte y sur
pbisectores_indec <- pbisectores_indec %>% 
  group_by(anio) %>% 
  mutate(agricultura_caza_silvicultura_y_pesca = sum(c(a_agricultura_ganaderia_caza_y_silvicultura,
                                                     b_pesca), na.rm = T),
         construccion_y_ega = sum(c(f_construccion,
                                    e_electricidad_gas_y_agua), na.rm = T),
         comercio_y_servicios = sum(pick(g_comercio_mayorista_minorista_y_reparaciones:p_hogares_privados_con_servicio_domestico),
                                    na.rm = T)
         ) %>% 
  ungroup()

#7 seleccion de datos nivel agregado
pbisectores_indec <- pbisectores_indec %>% 
  select(anio, producto_interno_bruto, agricultura_caza_silvicultura_y_pesca,
         explotacion_minas_y_canteras = c_explotacion_de_minas_y_canteras,
         industria_manufacturera = d_industria_manufacturera,
         construccion_y_ega, comercio_y_servicios)

#8 calculo de proporciones de vab por sector respecto al pib
pbisectores_indec <- pbisectores_indec %>%
  mutate(across(-anio, function(x) 100*x/producto_interno_bruto,
                .names = "prop_{.col}"))

#9 calculo las variaciones interanuales de las prop
pbisectores_indec <- pbisectores_indec %>%
  mutate(across(-c(anio, matches("prop_")), function(x) x/lag(x), .names = "var_{.col}"))

# pib bruto 2018
#10 se pasa la unidad de miles de pesos a pesos
pbiusd_fnys <- pbiusd_fnys %>% 
  filter(anio == 2018  & indicador == "PIB a precios de mercado" &
           unidad == "miles de US$") %>% 
  select(anio, iso3, pib = valor) %>% 
  mutate(pib = pib*1000)

#11 seleccion de variables de interes
pbisectores_fnys <- pbisectores_fnys %>% 
  filter(indicador %in% c(
    "PIB a costo de factores / precios básicos",
    "Agricultura, caza y silvicultura",
    "Pesca",
    "Explotación de minas y canteras",
    "Industrias Manufactureras",
    "Electricidad, Gas y Agua",
    "Construcción",
    "Comercio al por mayor y menor, y hoteles y restaurantes",
    "Transporte, almacenamiento y comunicaciones",
    "Intermediación financiera y actividades inmobiliarias - Total",
    "Administrac. pública y defensa y servicios sociales, comunales y personales - Total"
  ))


#14 filtro anios de interes (la serie llega a 2018)
pbisectores_fnys <- pbisectores_fnys %>%
  filter(anio %in% 1935:2018)

pbisectores_fnys <- pbisectores_fnys %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  janitor::clean_names()

#15 completo valores faltantes en pib_pmpb
# los años de 1970 a 1979 no tienen datos en la columna PIBpm%pb.
# Entiendo que esto es porque para esos años PIBpb%pm = 100,
# según se deduce de que la suma de los %pm de los sectores para esos años da igual a 100%
pbisectores_fnys <- pbisectores_fnys %>%
  rename(pib_pbpm = pib_a_costo_de_factores_precios_basicos) %>% 
  group_by(anio) %>% 
  mutate(pib_pbpm = ifelse(is.na(pib_pbpm), sum(c_across(-c(pib_pbpm)), na.rm = T ),
                           pib_pbpm)) %>% 
  ungroup()

#16 agrupo subsectores en grandes sectores
pbisectores_fnys <- pbisectores_fnys %>%
  group_by(anio) %>% 
  mutate(agricultura_caza_silvicultura_y_pesca = sum(c(agricultura_caza_y_silvicultura,pesca), na.rm = T),
         construccion_y_ega = sum(c(construccion,electricidad_gas_y_agua), na.rm = T),
         comercio_y_servicios = sum(c(comercio_al_por_mayor_y_menor_y_hoteles_y_restaurantes, 
                                      transporte_almacenamiento_y_comunicaciones,
                                      intermediacion_financiera_y_actividades_inmobiliarias_total,
                                      administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_total), na.rm = T)) %>% 
  select(anio, pib_pbpm, agricultura_caza_silvicultura_y_pesca,
         explotacion_minas_y_canteras = explotacion_de_minas_y_canteras,
         industria_manufacturera = industrias_manufactureras,
         construccion_y_ega, comercio_y_servicios)


#17 le agrego a los datos % de composicion de pib el dato de pib bruto 2018: columna pib
pbisectores_fnys <- left_join(pbisectores_fnys, pbiusd_fnys)

#18 calculo para anio 2018 el vab en millones de pesos como  % de sector a pb/pm /100 * valor pib pm bruto 
pbisectores_fnys <- pbisectores_fnys %>% 
  ungroup() %>% 
  mutate(across(-c(anio, iso3,  pib), function(x) {x/100*pib},
                .names = "bruto_{.col}"))

#19 reuno dataset fnys 1935:2018 con dataset de variaciones ia de indec 2019:2022
pbisectores_fnys <- pbisectores_fnys %>% 
  bind_rows(pbisectores_indec %>% filter(anio > 2018) %>% 
              select(anio, matches("var")))

#20 expando la serie de valores brutos de fnys usando las variaciones calculadas con dataset indec
# ver funcion expansor_xvar en aux_functions.R
pbisectores_fnys <- pbisectores_fnys %>%
  ungroup() %>% 
  arrange(anio) %>% 
  mutate(pib = expansor_xvar(pib, var_producto_interno_bruto),
         bruto_agricultura_caza_silvicultura_y_pesca = expansor_xvar(bruto_agricultura_caza_silvicultura_y_pesca,
                                                                     var_agricultura_caza_silvicultura_y_pesca),
         bruto_explotacion_minas_y_canteras = expansor_xvar(bruto_explotacion_minas_y_canteras,
                                                                     var_explotacion_minas_y_canteras),
         bruto_industria_manufacturera = expansor_xvar(bruto_industria_manufacturera,
                                                            var_industria_manufacturera),
         bruto_construccion_y_ega = expansor_xvar(bruto_construccion_y_ega,
                                                       var_construccion_y_ega),
         bruto_comercio_y_servicios = expansor_xvar(bruto_comercio_y_servicios,
                                                  var_comercio_y_servicios)
         )

 
#21 completo las vars de sector a pb como % de pib pm a partir de los valores brutos sectoriales expandidos con indec para 2019-2022 dividos valor pib pm bruto
pbisectores_fnys <- pbisectores_fnys %>%
  mutate(agricultura_caza_silvicultura_y_pesca = ifelse(is.na(agricultura_caza_silvicultura_y_pesca),
                                                        bruto_agricultura_caza_silvicultura_y_pesca/pib*100,
                                                        agricultura_caza_silvicultura_y_pesca),
         explotacion_minas_y_canteras = ifelse(is.na(explotacion_minas_y_canteras),
                                                        bruto_explotacion_minas_y_canteras/pib*100,
                                               explotacion_minas_y_canteras),
        industria_manufacturera = ifelse(is.na(industria_manufacturera),
                                         bruto_industria_manufacturera/pib*100,
                                                        industria_manufacturera),
        construccion_y_ega = ifelse(is.na(construccion_y_ega),
                                    bruto_construccion_y_ega/pib*100,
                                                        construccion_y_ega),
         comercio_y_servicios = ifelse(is.na(comercio_y_servicios),
                                                        bruto_comercio_y_servicios/pib*100,
                                                        comercio_y_servicios),
                                                        )

#22 completo 2019-2022 %pib pb/pm con la suma de los % de los sectores 
pbisectores_fnys <- pbisectores_fnys %>%
  group_by(anio) %>% 
  mutate(pib_pbpm = ifelse(is.na(pib_pbpm),
                                      sum(c_across(agricultura_caza_silvicultura_y_pesca:comercio_y_servicios)),
                           pib_pbpm)) %>% 
  ungroup() 

# seleccion variables
pbisectores_fnys <- pbisectores_fnys %>%
  select(anio, pib_pbpm, agricultura_caza_silvicultura_y_pesca:comercio_y_servicios)


#23 transformo los % de sector pb respecto pib pm a % sector pb respecto pib pb
df_output <- pbisectores_fnys %>%
  mutate(across(-c(anio, pib_pbpm), function(x) x/pib_pbpm*100)) %>% 
  ungroup() 



# Control vs output previo -------


df_output <- df_output %>% 
  select(-pib_pbpm) %>% 
  pivot_longer(cols = -c(anio), names_to = "sector", values_to = "valor") %>% 
  mutate(sector = case_when(
    grepl("agricultura", sector) ~ "Agricultura caza silvicultura y pesca",
    grepl("cantera", sector) ~ "Explotacion minas y canteras",
    grepl("construccion", sector) ~ "Construccion y Electricidad, Gas y Agua",
    grepl("comercio", sector) ~ "Comercio y servicios",
    grepl("industria", sector) ~ "Industria manufacturera",
    T ~ NA_character_
  ))


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio", "sector"),
  drop_joined_df = F
)



# Write output ------


df_output %>% 
  write_output(output_name = output_name, fuentes = c("R36C13", "R36C9", "R38C7"),
               subtopico = "ACECON",
               analista = "",
               pk = c("anio", "sector"),es_serie_tiempo = T, columna_indice_tiempo = "anio", nivel_agregacion = "pais",
               etiquetas_indicadores =  list("sector" = "Sector"),
               unidades = list("valor"="Porcentaje del PIB a precios básicos") )
