# Función para empalmar series temporales usando variaciones anuales
empalmar_series <- function(df_historico, df_oficial, 
                            var_empalme = "prop", 
                            grupos = c("cnt", "sector", "country"),
                            año_empalme = 2016) {
  
  library(dplyr)
  library(tidyr)
  
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

