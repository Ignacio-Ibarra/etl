#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

library(dplyr)
library(tidyr)

# Metadatos 
subtopico <- "INDUST"
output_name <- "peso_industria_empleo_prod_historico.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R35C106' # Puestos CGI 
fuente2 <- 'R453C296' # National Accounts. Analysis of Main Aggregates (AMA). Percentage Distribution (Shares) of GDP. All countries for all years - sorted alphabetically - Cuadro: Download-Shares-countries
fuente3 <- 'R231C102' # Groningen Growth and Development Centre
fuente4 <- "R36C13" # 
fuente5 <- "R38C7"


geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

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


# ordenar breakdown
df_break_tidy <- df_break %>% 
  dplyr::filter(stringr::str_detect(indicator_name,'ISIC D')) %>% 
  select(anio, iso3, pais_nombre, share_industrial_gdp = share ) 

# filtrar datos de empleo
df_empleo_tidy <- df_empleo %>% 
  filter(var_code == 'EMP' & !is.na(value)) %>% 
  group_by(year,cnt,sector) %>% 
  summarize(value = sum(value, na.rm = T)) %>% 
  group_by(year,cnt) %>% 
  mutate(prop = value / sum(value)) %>% 
  ungroup()


# Agregar empleo CGI 
df_cgi_tidy <- df_cgi %>% 
  filter(trim == 'Total', indicador %in% c('Total general','Industria manufacturera')) %>%  
  select(-c(letra,trim)) %>% 
  pivot_wider(names_from = indicador,values_from=puestos)%>% 
  mutate(prop = `Industria manufacturera` / `Total general`) %>% 
  select(year=anio,prop) %>% 
  mutate(cnt = 'ARG',
         sector = 'Manufacturing',
         value = NA_real_,
         country = 'Argentina')

# Tirar serie de argentina (CGI) para atras con GGDC
df_empleo_arg <- df_empleo_tidy %>% 
  filter(cnt == 'ARG', sector == 'Manufacturing')


empalmar_series <- function(df_historico, df_oficial, 
                            var_empalme = "prop", 
                            grupos = c("cnt", "sector", "country"),
                            año_empalme = 2016) {
  
  
  
  # 1. Calcular variaciones anuales en la serie histórica
  print("Paso 1: Calculando variaciones anuales...")
  
  df_variaciones <- df_historico %>%
    arrange(across(all_of(grupos)), year) %>%
    group_by(across(all_of(grupos))) %>%
    mutate(
      # Calcular variación anual (tasa de crecimiento)
      variacion_anual = ifelse(lag(.data[[var_empalme]]) != 0 & !is.na(lag(.data[[var_empalme]])),
                               (.data[[var_empalme]] / lag(.data[[var_empalme]])) - 1,
                               NA),
      # También calcular el factor multiplicativo
      factor_crecimiento = ifelse(!is.na(variacion_anual), 
                                  1 + variacion_anual, 
                                  NA)
    ) %>%
    ungroup() %>%
    filter(!is.na(variacion_anual)) %>%
    select(all_of(c("year", grupos, "variacion_anual", "factor_crecimiento")))
  
  print(paste("Variaciones calculadas para", nrow(df_variaciones), "observaciones"))
  
  # 2. Obtener valores de referencia del año de empalme en la serie oficial
  print("Paso 2: Obteniendo valores de referencia...")
  
  df_referencia <- df_oficial %>%
    filter(year == año_empalme) %>%
    select(all_of(c(grupos, var_empalme))) %>%
    rename(valor_referencia = !!sym(var_empalme))
  
  print(paste("Valores de referencia obtenidos para", nrow(df_referencia), "grupos"))
  
  # 3. Crear serie empalmada hacia atrás
  print("Paso 3: Empalmando series hacia atrás...")
  
  # Obtener años únicos menores al año de empalme, ordenados de mayor a menor
  años_empalme <- sort(unique(df_variaciones$year[df_variaciones$year < año_empalme]), 
                       decreasing = TRUE)
  
  # Inicializar con valores de referencia
  serie_empalmada <- df_referencia %>%
    mutate(year = año_empalme,
           !!sym(var_empalme) := valor_referencia) %>%
    select(-valor_referencia)
  
  # Empalmar año por año hacia atrás
  for(año in años_empalme) {
    print(paste("Procesando año:", año))
    
    # Obtener variaciones para el año siguiente (año + 1)
    variaciones_año <- df_variaciones %>%
      filter(year == año + 1) %>%
      select(all_of(c(grupos, "factor_crecimiento")))
    
    if(nrow(variaciones_año) > 0) {
      # Calcular valores hacia atrás usando la variación
      valores_año <- serie_empalmada %>%
        filter(year == año + 1) %>%
        inner_join(variaciones_año, by = grupos) %>%
        mutate(
          year = año,
          !!sym(var_empalme) := .data[[var_empalme]] / factor_crecimiento
        ) %>%
        select(-factor_crecimiento)
      
      # Agregar a la serie empalmada
      serie_empalmada <- bind_rows(serie_empalmada, valores_año)
    }
  }
  
  # 4. Combinar con la serie oficial (2016 en adelante)
  print("Paso 4: Combinando con serie oficial...")
  
  serie_oficial_completa <- df_oficial %>%
    select(all_of(c("year", grupos, var_empalme, "value")))
  
  # Crear serie final
  serie_final <- bind_rows(
    # Serie empalmada (años anteriores al empalme)
    serie_empalmada %>% 
      filter(year < año_empalme) %>%
      mutate(fuente = "empalmada"),
    
    # Serie oficial (año de empalme en adelante)
    serie_oficial_completa %>%
      mutate(fuente = "oficial")
  ) %>%
    arrange(across(all_of(grupos)), year)
  
  print("Proceso completado exitosamente!")
  
  # Estadísticas del resultado
  cat("\n=== RESUMEN DEL EMPALME ===\n")
  cat("Años empalmados:", min(serie_final$year[serie_final$fuente == "empalmada"]), 
      "a", año_empalme - 1, "\n")
  cat("Años oficiales:", año_empalme, "a", max(serie_final$year), "\n")
  cat("Total de observaciones:", nrow(serie_final), "\n")
  cat("Grupos únicos:", nrow(distinct(serie_final, across(all_of(grupos)))), "\n")
  
  return(list(
    serie_empalmada = serie_final,
    variaciones_utilizadas = df_variaciones,
    valores_referencia = df_referencia
  ))
}



# Ejecutar el empalme
resultado <- empalmar_series(
  df_historico = df_empleo_arg,
  df_oficial = df_cgi_tidy,
  var_empalme = "prop",
  grupos = c("cnt", "sector"),
  año_empalme = 2016
)

# Obtener la serie final
serie_final <- resultado$serie_empalmada %>% 
  select(-c(value, fuente))


df_empleo_final <- serie_final %>% 
  bind_rows(
    df_empleo_tidy %>% 
      dplyr::filter(sector == "Manufacturing", cnt != "ARG") %>% 
      select(-value)
  )


df_break_final <- df_break_tidy %>% 
  dplyr::filter(iso3 != "ARG") %>% 
  select(geocodigoFundar = iso3, anio, share_industrial_gdp ) %>% 
  bind_rows(
    df_break_arg %>% 
      dplyr::filter(sector == "industria_manufacturera") %>% 
      mutate(geocodigoFundar = "ARG") %>% 
      select(anio, geocodigoFundar, share_industrial_gdp = valor)
  ) %>% 
  mutate(share_industrial_gdp = share_industrial_gdp /100)


# Agregar tasas de gdp industry
df_output <- df_empleo_final %>% 
  select(anio = year, geocodigoFundar = cnt, share_industrial_employment = prop) %>% 
  full_join(df_break_final, 
            join_by(anio, geocodigoFundar)) %>% 
  arrange(geocodigoFundar, anio) %>% 
  left_join(geo_front, join_by(geocodigoFundar)) %>% 
  pivot_longer(
    matches("share.*"), 
    names_to = 'variable',
    values_to = 'valor'
  ) %>% 
  drop_na(valor)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 


pks_comparacion <- c('anio','geocodigoFundar', 'variable')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
)



armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")






