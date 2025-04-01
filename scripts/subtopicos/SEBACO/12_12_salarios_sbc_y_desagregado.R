#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "12_salarios_sbc_y_desagregado"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R314C183' # OEDE - salarios mensual 4 digitos CIIU Rev 3
fuente2 <- 'R276C164' # OEDE - empleo trimestral 4 dígitos CIIU Rev 3
fuente3 <- 'R313C0' # BCRA - tasa inflacion mensual. 
fuente4 <- 'R127C54' # INDEC - inflacion mensual
fuente5 <- 'R124C50' # IPC San Luis


df_oede_salarios <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  mutate(
    anio = year(fecha),
    mes = month(fecha),
    trimestre = quarter(fecha)
  ) %>% 
  mutate(
    ciiu_rev3_4d = ifelse(rama_de_actividad == "Total", 9999, ciiu_rev3_4d)
  ) %>% 
  select(-cuadro,-rama_de_actividad)

df_oede_empleo <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  mutate(
    ciiu_rev3_4d = ifelse(rama_de_actividad == "Total", 9999, ciiu_rev3_4d)
  ) %>% 
  select(-cuadro,-rama_de_actividad)


df_oede_salario_empleo <- df_oede_empleo %>% 
  left_join(df_oede_salarios, join_by(anio, trimestre, ciiu_rev3_4d)) %>% 
  group_by(ciiu_rev3_4d, anio) %>% 
  dplyr::filter(n() == 12) %>% 
  ungroup()


df_bcra <- argendataR::get_raw_path(fuente3) %>% 
  readr::read_csv() %>% 
  mutate(
    anio = year(fecha),
    mes = month(fecha)
  ) %>% 
  select(fecha, anio, mes, mensual_bcra = valor)


df_indec <- argendataR::get_clean_path(fuente4) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(region == "Nacional", codigo == 0, anio > 2016) %>% 
  select(anio, mes, mensual_indec = v_m_ipc)


df_san_luis <- argendataR::get_clean_path(fuente5) %>% 
  arrow::read_parquet() %>% 
  select(anio, mes, mensual_san_luis = var_mes_mes_porcentual)


infla_mensual <- df_bcra %>%
  left_join(df_san_luis, join_by(anio, mes)) %>% 
  left_join(df_indec, join_by(anio, mes)) %>% 
  mutate(
    tasa_inflacion = case_when(
      anio >= 2017 ~ mensual_indec,
      anio > 2005 ~ mensual_san_luis,
      TRUE ~ mensual_bcra
        
    )
  ) 


anio_min <- min(df_oede_salario_empleo$anio)

anio_max <- max(df_oede_salario_empleo$anio)


deflactor_mensual <- infla_mensual %>% 
  dplyr::filter(anio <= anio_max, anio >= anio_min) %>% 
  arrange(fecha) %>% 
  mutate(indice_precios = cumprod(1 + tasa_inflacion / 100)) %>% 
  mutate(base = last(indice_precios), # Índice base es el último valor del índice
         deflactor = indice_precios / base) %>% 
  select(anio, mes, fecha, deflactor)


# Se toma como anio mes base el mes de diciembre del ultimo año completo. 
fecha_base <- max(deflactor_mensual$fecha)
anio_mes_base <- paste0("Año: ", year(fecha_base)," - Mes: ", month(fecha_base))

df_salarios_constantes_oede_anual <- df_oede_salario_empleo %>% 
  select(-fecha) %>% 
  left_join(deflactor_mensual, join_by(anio, mes)) %>%     # Filtro anios y meses
  mutate(
    salario_promedio_constante = salario_promedio_puestos_privados / deflactor
  ) %>% 
  select(ciiu_rev3_4d, anio, mes, trimestre, salario_promedio_constante, cant_promedio_puestos_privados) %>% 
  group_by(ciiu_rev3_4d, anio) %>%
  summarise(
    salario_promedio_constante = mean(salario_promedio_constante, na.rm = T),
    empleo_promedio = mean(cant_promedio_puestos_privados, na.rm = T)
  ) %>% 
  ungroup() 


codigos_sbc <- c(
  '2213', # Edición de grabaciones
  '3530', # Fabricación y reparación de aeronaves (????)
  '7210', # Servicios de consultores en equipo de informática
  '7220', # Servicios de consultores en informática y suministros de programas de informática
  '7230', # Procesamiento de datos
  '7240', # Servicios relacionados con bases de datos
  '7290', # Actividades de informática n.c.p.
  '7300', # Investigación y desarrollo
  '7410', # Servicios jurídicos y de contabilidad, teneduría de libros y auditoría; asesoramiento en materia de impuestos; estudios de mercados y realización de encuestas de opinión pública; asesoramiento empresarial y en materia de gestión
  '7421', # Servicios de arquitectura e ingeniería y servicios conexos de asesoramiento técnico
  '7430', # Servicios de publicidad
  '7494', # Servicios de fotografía
  '7491', # Obtención y dotación de personal
  '9211'  # Producción y distribución de filmes y videocintas
)



df_salario_economia_anual <- df_salarios_constantes_oede_anual %>% 
  dplyr::filter(ciiu_rev3_4d == 9999) %>% 
  mutate(sector = "Promedio economía") %>% 
  select(anio, sector, salario_ponderado_cte = salario_promedio_constante)


df_salario_sectores_SBC_anual <- df_salarios_constantes_oede_anual %>% 
  dplyr::filter(ciiu_rev3_4d %in% codigos_sbc) %>%
  mutate(
    sector = case_when(ciiu_rev3_4d %in% c('7210','7220','7230','7240','7290') ~ 'SSI',
                       ciiu_rev3_4d %in% c('7300') ~ 'Investigación y desarrollo',
                       ciiu_rev3_4d %in% c('7410') ~ 'Ss. Jurídicos y de contabilidad',
                       ciiu_rev3_4d %in% c('7421') ~ 'Ss. Arquitectura',
                       ciiu_rev3_4d %in% c('7430') ~ 'Ss. publicidad',
                       TRUE ~ 'Otras')
  ) %>% 
  group_by(anio, sector) %>% 
  summarise(
    salario_ponderado_cte = stats::weighted.mean(salario_promedio_constante,
                                                          empleo_promedio)
  ) %>% 
  ungroup()


df_salario_sector_SBC_entero_anual <-  df_salarios_constantes_oede_anual %>% 
  dplyr::filter(ciiu_rev3_4d %in% codigos_sbc) %>%
  mutate(
    sector = "SBC"
  ) %>% 
  group_by(anio, sector) %>% 
  summarise(
    salario_ponderado_cte = stats::weighted.mean(salario_promedio_constante,
                                                          empleo_promedio)
  ) %>% 
  ungroup()


df_output <- bind_rows(df_salario_economia_anual, 
                       df_salario_sector_SBC_entero_anual, 
                       df_salario_sectores_SBC_anual)


###########################################################################
# ARMO DATASET COMPARABLE DADO QUE EL CAMBIO DE LA FECHA BASE MODIFICA TODO
###########################################################################


fecha_max_comparacion <- as.Date("2023-09-30") # fecha nico. 


deflactor_mensual_comparacion <- infla_mensual %>% 
  dplyr::filter(fecha <= fecha_max_comparacion, anio >= anio_min) %>% 
  arrange(fecha) %>% 
  mutate(indice_precios = cumprod(1 + tasa_inflacion / 100)) %>% 
  mutate(base = last(indice_precios), # Índice base es el último valor del índice
         deflactor = indice_precios / base) %>% 
  select(anio, mes, fecha, deflactor) %>% 
  group_by(anio) %>% 
  dplyr::filter(n() == 12) %>% 
  ungroup()


df_salarios_constantes_oede_comparacion <- df_oede_salario_empleo %>% 
  select(-fecha) %>%  
  right_join(deflactor_mensual_comparacion, join_by(anio, mes)) %>% 
  mutate(
    salario_promedio_constante = salario_promedio_puestos_privados / deflactor
    ) %>% 
  select(ciiu_rev3_4d, anio, mes, trimestre, salario_promedio_constante, cant_promedio_puestos_privados) %>% 
  group_by(ciiu_rev3_4d, anio) %>% 
  summarise(
    salario_promedio_constante = mean(salario_promedio_constante),
    empleo_promedio = mean(cant_promedio_puestos_privados)
  ) %>% 
  ungroup() 


df_salario_economia_anual_comparacion <- df_salarios_constantes_oede_comparacion %>% 
  dplyr::filter(ciiu_rev3_4d == 9999) %>% 
  mutate(sector = "Promedio economía") %>% 
  select(anio, sector, salario_ponderado_cte = salario_promedio_constante)


df_salario_sectores_SBC_anual_comparacion <- df_salarios_constantes_oede_comparacion %>% 
  dplyr::filter(ciiu_rev3_4d %in% codigos_sbc) %>%
  mutate(
      sector = case_when(ciiu_rev3_4d %in% c('7210','7220','7230','7240','7290') ~ 'SSI',
                         ciiu_rev3_4d %in% c('7300') ~ 'Investigación y desarrollo',
                         ciiu_rev3_4d %in% c('7410') ~ 'Ss. Jurídicos y de contabilidad',
                         ciiu_rev3_4d %in% c('7421') ~ 'Ss. Arquitectura',
                         ciiu_rev3_4d %in% c('7430') ~ 'Ss. publicidad',
                         TRUE ~ 'Otras')
    ) %>% 
  group_by(anio, sector) %>% 
  summarise(
    salario_ponderado_cte = stats::weighted.mean(salario_promedio_constante,
                                                          empleo_promedio)
  ) %>% 
  ungroup()


df_salario_sector_SBC_entero_anual_comparacion <-  df_salarios_constantes_oede_comparacion %>% 
  dplyr::filter(ciiu_rev3_4d %in% codigos_sbc) %>%
  mutate(
    sector = "SBC"
  ) %>% 
  group_by(anio, sector) %>% 
  summarise(
    salario_ponderado_cte = stats::weighted.mean(salario_promedio_constante,
                                                          empleo_promedio)
  ) %>% 
  ungroup()


df_comparable <- bind_rows(df_salario_economia_anual_comparacion, 
                         df_salario_sector_SBC_entero_anual_comparacion, 
                         df_salario_sectores_SBC_anual_comparacion)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(salario_ponderado_cte = salario_promedio,
         sector = sector_sbc)

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_comparable,
  nombre = output_name,
  pk = c('anio','sector'),
  drop_joined_df =  F
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
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output

etiquetas_nuevas <- data.frame(
  variable_nombre = c("sector",
                      "salario_ponderado_cte"),
  descripcion = c("Sectores agregados de distintas ramas del sector servicios basados en conocimientos (ver detalle en aclaración)",
                  glue::glue("Salario promedio a valores constantes de {anio_mes_base}"))
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


aclaracion <- paste0(
  
  c("Se cambió la fecha base, antes era Septiembre 2023, ahora será el mes de Diciembre del ultimo año completo en OEDE. ",
    "El salario constante por sector se calcula primero a nivel de 4 dígitos del CIIU Rev 3, deflactado por la inflación mensual. ",
    "Luego se obtiene un salario promedio para los sectores agregados, utilizando como ponderador la cantidad de empleo registrado para cada sector a 4 dígitos. ",
    "Los sectores son: 'Servicios de software e informática (SSI)' que incluye los sectores '7210','7220','7230','7240','7290', ",
    "'Investigación y desarrollo' que incluye el sector '7300', 'Ss. Jurídicos y de contabilidad' que incluye '7410', ",
    "'Ss. Arquitectura' que incluye '7421', 'Ss. publicidad' que incluye '7430' y 'Otras' que incluye '2213','3530','7494','7491','9211'."),
  
  collapse = ""
 )


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    aclaraciones = aclaracion,
    cambio_nombre_cols = list('salario_ponderado_cte' = 'salario_promedio',
                              'sector' = 'sector_sbc'),
    unidades = list("salario_ponderado_cte" = glue::glue("en pesos constantes de {anio_mes_base}"))
  )
