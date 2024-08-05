# 7_pib_comp_va
# descripcion

# vars config del script
output_name <- "7_pib_comp_va.csv"
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

# me quedo solo con los datos de total anual
pbisectores_indec <- pbisectores_indec %>% 
  filter(trim == "Total") %>% 
  select(-trim) 

# selecciono anios que corresponden a la expansion
pbisectores_indec <- pbisectores_indec %>%
  filter(anio %in% 2004:max(anio))

# seleccion de variables de interes
pbisectores_indec %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  colnames()

pbisectores_indec <- pbisectores_indec %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  select(anio,
         valor_agregado_bruto_a_precios_basicos,
         a_agricultura_ganaderia_caza_y_silvicultura,
         b_pesca, c_explotacion_de_minas_y_canteras, d_industria_manufacturera,
         e_electricidad_gas_y_agua, f_construccion,
         g_comercio_mayorista_minorista_y_reparaciones,h_hoteles_y_restaurantes, i_transporte_almacenamiento_y_comunicaciones,
         j_intermediacion_financiera, k_actividades_inmobiliarias_empresariales_y_de_alquiler,
         l_administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria,
         m_ensenanza, n_servicios_sociales_y_de_salud,
         o_otras_actividades_de_servicios_comunitarias_sociales_y_personales, p_hogares_privados_con_servicio_domestico)


# calculo de proporciones de vab por sector respecto al vab total
pbisectores_indec <- pbisectores_indec %>%
  mutate(across(-anio, function(x) 100*x/valor_agregado_bruto_a_precios_basicos,
                .names = "prop_{.col}"))


## datos fnys 
# seleccion de variables de interes
pbisectores_fnys <- pbisectores_fnys %>% 
  filter(! indicador %in% c(
    "PIB Total a precios de mercado",
    "PIB a costo de factores / precios básicos"
  ))


# filtro anios de interes (la serie llega a 2018)
pbisectores_fnys <- pbisectores_fnys %>%
  filter(anio %in% 1935:2004)

# pivot
pbisectores_fnys <- pbisectores_fnys %>%
  pivot_wider(names_from = indicador, values_from = valor) %>% 
  janitor::clean_names()


# la serie de pesca tiene datos faltantes
# se imputan con extrapolacion lineal
pbisectores_fnys$pesca2 <- pbisectores_fnys$pesca
pbisectores_fnys$pesca2[which(is.na(pbisectores_fnys$pesca))] <-  zoo::na.approx(pbisectores_fnys$pesca, xout = which(is.na(pbisectores_fnys$pesca)))

# en los anios donde habia datos faltantes para pesca es necesario restar el valor imputado a agricultura
# esto es xq entendemos que al sector agro se le sumo el dato de pesca para esos anios 
pbisectores_fnys$agricultura_caza_y_silvicultura[which(is.na(pbisectores_fnys$pesca))] <- pbisectores_fnys$agricultura_caza_y_silvicultura[which(is.na(pbisectores_fnys$pesca))] -pbisectores_fnys$pesca2[which(is.na(pbisectores_fnys$pesca))] 

# en algunos subsectores habia datos faltantes
# se calcula la proporcion del subsector sobre el sector y se extrapola la proporcion para datos faltantes
# se recalcula el valor del subsector como proporcion*total sector
pbisectores_fnys <- pbisectores_fnys %>% 
  mutate(prop_intermediacion_finan = intermediacion_financiera_y_actividades_inmobiliarias_intermediacion_financiera/intermediacion_financiera_y_actividades_inmobiliarias_total,
         prop_actividades_inmobiliarias = intermediacion_financiera_y_actividades_inmobiliarias_act_inmobiliarias_empresariales_y_de_alquiler/intermediacion_financiera_y_actividades_inmobiliarias_total,
         prop_admin_pub = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_admin_publica_defensa_y_org_extraterr/administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_total,
         prop_servicios_soc = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_otros_servicios/administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_total) 


columnas_prop <- grep("^prop_",colnames(pbisectores_fnys), value = T)


for(i in columnas_prop) {
  
    pbisectores_fnys[which(is.na(pbisectores_fnys[i])),i] <- zoo::na.approx(pbisectores_fnys[[i]],
                                                                            xout = which(is.na(pbisectores_fnys[[i]])))

  }


pbisectores_fnys <- pbisectores_fnys %>% 
  mutate(
    intermediacion_financiera_y_actividades_inmobiliarias_intermediacion_financiera = prop_intermediacion_finan*intermediacion_financiera_y_actividades_inmobiliarias_total,
    intermediacion_financiera_y_actividades_inmobiliarias_act_inmobiliarias_empresariales_y_de_alquiler = prop_actividades_inmobiliarias*intermediacion_financiera_y_actividades_inmobiliarias_total,
    administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_admin_publica_defensa_y_org_extraterr = prop_admin_pub*administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_total,
    administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_otros_servicios = prop_servicios_soc*administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_total
    
  )


pbisectores_fnys <- pbisectores_fnys %>% 
  mutate(pesca = pesca2) %>% 
  select(-c("pesca2", 
            starts_with("prop_"), ends_with("_total")))

# se calcula la proporcion de cada sector sobre el total vab
pbisectores_fnys <- pbisectores_fnys %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(-anio)))

pbisectores_fnys <- pbisectores_fnys %>% 
  mutate(across(-c(anio, total), function(x) 100*x/total)) %>% 
  select(-total)



# junto las series
# seleccion de columnas limpieza de nombres y pivot longer
pbisectores_indec <- pbisectores_indec %>% 
  select(anio, starts_with("prop"))

pbisectores_indec <- pbisectores_indec %>% 
  rowwise() %>% 
  mutate(prop_comercio_mayorista_minorista_hoteles_restaurantes = sum(c_across(c(prop_h_hoteles_y_restaurantes, 
                                                                                 prop_g_comercio_mayorista_minorista_y_reparaciones))),
         prop_otros_servicios = sum(c_across(prop_m_ensenanza:prop_p_hogares_privados_con_servicio_domestico)))

pbisectores_indec <- pbisectores_indec %>% 
  select(-c(prop_g_comercio_mayorista_minorista_y_reparaciones,
            prop_h_hoteles_y_restaurantes, prop_m_ensenanza:prop_p_hogares_privados_con_servicio_domestico))

colnames(pbisectores_indec) <- gsub("prop_._|prop_", "",
     colnames(pbisectores_indec))

pbisectores_indec <- pbisectores_indec %>% 
  select(-producto_interno_bruto)

pbisectores_fnys <- pbisectores_fnys %>% 
  rename( otros_servicios = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_otros_servicios,
         administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_admin_publica_defensa_y_org_extraterr,
         intermediacion_financiera = intermediacion_financiera_y_actividades_inmobiliarias_intermediacion_financiera,
         actividades_inmobiliarias_empresariales_y_de_alquiler = intermediacion_financiera_y_actividades_inmobiliarias_act_inmobiliarias_empresariales_y_de_alquiler,
         comercio_mayorista_minorista_hoteles_restaurantes = comercio_al_por_mayor_y_menor_y_hoteles_y_restaurantes,
         industria_manufacturera =industrias_manufactureras,
         agricultura_ganaderia_caza_y_silvicultura = agricultura_caza_y_silvicultura,
          )



df_output <- bind_rows(pbisectores_fnys,
                        pbisectores_indec) 


df_output %>% 
  filter(anio == 2004) %>% 
  pivot_longer(cols = -anio) %>% 
  group_by(name) %>% 
  summarise(sd = stats::sd(value)/mean(value))

df_output <- df_output %>% 
  pivot_longer(cols = -anio, names_to = "sector", values_to = "valor")

df_output <- df_output %>% 
  mutate(valor = round(valor, 4)) %>% 
  distinct()


# el control por el momento no tiene version previa de comparacion

# df_anterior <- argendataR::descargar_output(nombre = output_name,
#                                             subtopico = subtopico,
#                                             entrega_subtopico = "primera_entrega")
# 
# #-- Controlar Output ----
# 
# comparacion <- argendataR::comparar_outputs(
#   df_output,
#   df_anterior,
#   pk = c("anio", "sector"),
#   drop_joined_df = F
# )



# Write output ------


df_output %>% 
  write_output(output_name = output_name,
               fuentes = c("R36C13", "R36C9", "R38C7"),
               subtopico = "ACECON",
               analista = "",
               pk = c("anio", "sector"),
               es_serie_tiempo = T,
               columna_indice_tiempo = "anio",
               nivel_agregacion = "pais",
               etiquetas_indicadores =  list("sector" = "Sector"),
               unidades = list("valor"="Porcentaje del PIB a precios básicos") )
