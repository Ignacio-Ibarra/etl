# 7_pib_comp_va
# descripcion

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ACECON"
output_name <- "7_pib_comp_va"
analista = "Daniel Schteingart y Juan Gabriel Juara"
fuente1 <- "R36C13"
fuente2 <- "R38C7"



# sectores a precios basicos como % del pib a precio de mercado y PIB a precio de basicos como % respecto a precio de mercado
pbisectores_fnys <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

# vab por sectores a precios corrientes en millones de pesos corrientes 
pbisectores_indec <- arrow::read_parquet(argendataR::get_clean_path(fuente2))


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



df_output <- bind_rows(pbisectores_fnys,
                        pbisectores_indec) 


df_output <- df_output %>% 
  pivot_longer(cols = -anio, names_to = "sector", values_to = "valor")

df_output <- df_output %>% 
  mutate(valor = round(valor, 4)) %>% 
  distinct() %>% drop_na(valor)


# el control por el momento no tiene version previa de comparacion

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega")
# 
# #-- Controlar Output ----
# 
comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("anio", "sector"),
  drop_joined_df = F
)



#-- Exportar Output ----

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
  dplyr::filter(grepl(paste0(output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("anio","sector", "valor"),
  descripcion = c("Año de referencia",
                  "Sector de la actividad económica",
                  "Participación en el VAB a precios básicos, en pesos corrientes")
)

descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio", "sector"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("valor"="porcentaje")
  )


