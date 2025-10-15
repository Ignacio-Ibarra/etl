#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "11_peso_industria_empleo_historico"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R35C106' # Puestos CGI 
fuente2 <- 'R453C296' # National Accounts. Analysis of Main Aggregates (AMA). Percentage Distribution (Shares) of GDP. All countries for all years - sorted alphabetically - Cuadro: Download-Shares-countries
fuente3 <- 'R231C102' # Groningen Growth and Development Centre
fuente4 <- "R36C13"
fuente5 <- "R38C7"


df_cgi <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_break <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() 

df_empleo <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet() 


# BREAKS ARGENTINA


# sectores a precios basicos como % del pib a precio de mercado y PIB a precio de basicos como % respecto a precio de mercado
pbisectores_fnys <- arrow::read_parquet(argendataR::get_clean_path(fuente4))

# vab por sectores a precios corrientes en millones de pesos corrientes 
pbisectores_indec <- arrow::read_parquet(argendataR::get_clean_path(fuente5))


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


pbisectores_fnys <- pbisectores_fnys %>% 
  rename( otros_servicios = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_otros_servicios,
          administracion_publica_y_defensa_planes_de_seguridad_social_de_afiliacion_obligatoria = administrac_publica_y_defensa_y_servicios_sociales_comunales_y_personales_admin_publica_defensa_y_org_extraterr,
          intermediacion_financiera = intermediacion_financiera_y_actividades_inmobiliarias_intermediacion_financiera,
          actividades_inmobiliarias_empresariales_y_de_alquiler = intermediacion_financiera_y_actividades_inmobiliarias_act_inmobiliarias_empresariales_y_de_alquiler,
          comercio_mayorista_minorista_hoteles_restaurantes = comercio_al_por_mayor_y_menor_y_hoteles_y_restaurantes,
          industria_manufacturera =industrias_manufactureras,
          agricultura_ganaderia_caza_y_silvicultura = agricultura_caza_y_silvicultura,
  )



df_break_arg <- bind_rows(pbisectores_fnys,
                       pbisectores_indec) %>% 
  pivot_longer(cols = -anio, names_to = "sector", values_to = "valor") %>% 
  mutate(valor = round(valor, 4)) %>% 
  distinct() %>% drop_na(valor)



# carga de fuentes - nico 
df_break <- dplyr::as_tibble(openxlsx::read.xlsx('https://unstats.un.org/unsd/amaapi/api/file/22',startRow = 3))
df_empleo <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/ETD_10SD_EASD_CLEAN.parquet')
df_cgi <- arrow::read_parquet('http://149.50.137.164:2147/static/etl-fuentes/clean/puestos_serie_cgi_CLEAN.parquet')
df_break_arg <- read_csv('https://raw.githubusercontent.com/argendatafundar/data/main/ACECON/7_pib_comp_va.csv')

# ordenar breakdown
df_break <- df_break %>% 
  dplyr::filter(stringr::str_detect(IndicatorName,'ISIC D'))
df_break <- df_break %>% 
  tidyr::pivot_longer(-c(CountryID,Country,IndicatorName),values_to='industry_gdp',names_to='year')
df_break$IndicatorName <- NULL
df_break$CountryID <- NULL
df_break$year <- as.numeric(df_break$year)

# filtrar datos de empleo
df_empleo <- df_empleo %>% 
  filter(var_code == 'EMP' & !is.na(value))

# calcular participacion industria empleo 
df_empleo <- df_empleo %>% 
  group_by(year,cnt,sector) %>% 
  summarize(value = sum(value))
df_empleo <- df_empleo %>% 
  group_by(year,cnt) %>% 
  mutate(prop = value / sum(value))

# Agregar nombre de pais 
df_empleo <- df_empleo %>% 
  left_join(dicc_pais,by=c('cnt'='iso3c'))

# Agregar empleo CGI 
df_cgi <- df_cgi %>% 
  filter(trim == 'Total')
df_cgi <- df_cgi %>% 
  filter(indicador %in% c('Total general','Industria manufacturera'))
df_cgi <- df_cgi %>%  
  select(-c(letra,trim)) %>% 
  pivot_wider(names_from = indicador,values_from=puestos)
df_cgi <- df_cgi %>% 
  mutate(prop = `Industria manufacturera` / `Total general`)
df_cgi <- df_cgi %>% 
  select(year=anio,prop) %>% 
  mutate(cnt = 'ARG',
         sector = 'Manufacturing',
         value = NA_real_,
         country = 'Argentina')

# Tirar serie de argentina (CGI) para atras con GGDC
df_empleo_arg <- df_empleo %>% 
  filter(country == 'Argentina')
df_empleo_arg <- df_empleo_arg %>% 
  filter(sector == 'Manufacturing')

# Ejecutar el empalme
resultado <- empalmar_series(
  df_historico = df_empleo_arg,
  df_oficial = df_cgi,
  var_empalme = "prop",
  grupos = c("cnt", "sector", "country"),
  año_empalme = 2016
)

# Obtener la serie final
serie_final <- resultado$serie_empalmada
serie_final$fuente <- NULL

# Juntar la serie 
df_empleo$value <- NULL
df_empleo <- df_empleo %>% 
  filter(cnt != 'ARG')
serie_final$value <- NULL
df_empleo <- bind_rows(df_empleo,serie_final)

# Armar dato de share de PIB Industrial en Argentina 
df_break <- df_break %>% 
  filter(Country != 'Argentina')
df_break_arg <- df_break_arg %>% 
  filter(sector == 'industria_manufacturera')
df_break_arg <- df_break_arg %>% 
  mutate(Country = 'Argentina') %>% 
  select(-sector)
df_break_arg <- df_break_arg %>% 
  select(Country,year=anio,industry_gdp = valor)
df_break <- bind_rows(df_break,df_break_arg)

# Agregar tasas de gdp industry
df_final <- df_empleo %>% 
  filter(sector == 'Manufacturing') %>% 
  select(-sector) %>% 
  full_join(df_break,by=c('country'='Country','year'='year')) %>% 
  mutate(cnt = if_else(country == 'Argentina','ARG',cnt))

# Filtrar manufacturas
df_final <- df_final %>% 
  rename(share_industrial_employment=prop,
         share_industrial_gdp = industry_gdp) %>% 
  mutate(share_industrial_gdp = share_industrial_gdp / 100)

# Pivotear 
df_final <- df_final %>% 
  pivot_longer(-c(year,cnt,country),names_to='variable',values_to='valor')

# Sacar nulos 
df_final <- df_final %>% 
  filter(!is.na(valor))

# guardar resultados 
readr::write_csv(df_final,file.path(outstub,'11_peso_industria_empleo_prod_historico.csv'))
