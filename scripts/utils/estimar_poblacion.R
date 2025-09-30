# Código R para interpolación acumulada de población argentina a largo plazo
# Método: Interpolación geométrica/exponencial (más realista para población)

library(dplyr)
library(ggplot2)
library(readr)

poblacion_completa_dato <- function(){
  # 1. CARGAR DATOS
  url <- "https://raw.githubusercontent.com/argendatafundar/data/main/DEMOGR/poblacion_arg_largo_plazo.csv"
  poblacion_raw <- read_csv(url)
  
  print("Dataset original:")
  print(paste("Rango de años:", min(poblacion_raw$anio), "a", max(poblacion_raw$anio)))
  print(paste("Total de observaciones:", nrow(poblacion_raw)))
  
  # 2. IDENTIFICAR GAPS EN LA SERIE TEMPORAL
  # Crear secuencia completa de años
  anios_completos <- seq(min(poblacion_raw$anio), max(poblacion_raw$anio), by = 1)
  anios_faltantes <- setdiff(anios_completos, poblacion_raw$anio)
  
  print(paste("Años faltantes:", length(anios_faltantes)))
  print("Principales gaps:")
  if (length(anios_faltantes) > 0) {
    # Identificar grupos consecutivos de años faltantes
    diff_anios <- c(1, diff(anios_faltantes))
    grupos <- cumsum(diff_anios > 1)
    gaps_info <- split(anios_faltantes, grupos)
    
    for (i in 1:length(gaps_info)) {
      gap <- gaps_info[[i]]
      print(paste("Gap", i, ":", min(gap), "-", max(gap), "(", length(gap), "años)"))
    }
  }
  
  # 3. FUNCIÓN DE INTERPOLACIÓN ACUMULADA (GEOMÉTRICA/EXPONENCIAL)
  interpolacion_geometrica <- function(x1, y1, x2, y2, x_inter) {
    # x1, y1: punto inicial
    # x2, y2: punto final  
    # x_inter: años a interpolar
    
    # Calcular tasa de crecimiento anual compuesta
    n_years <- x2 - x1
    tasa_crecimiento <- (y2 / y1)^(1/n_years) - 1
    
    # Aplicar crecimiento geométrico
    valores_inter <- y1 * (1 + tasa_crecimiento)^(x_inter - x1)
    
    return(valores_inter)
  }
  
  # Función alternativa: interpolación exponencial suavizada
  interpolacion_exponencial <- function(x1, y1, x2, y2, x_inter) {
    # Transformar a escala logarítmica para linearizar el crecimiento exponencial
    log_y1 <- log(y1)
    log_y2 <- log(y2)
    
    # Interpolación lineal en escala log
    log_y_inter <- log_y1 + (log_y2 - log_y1) * (x_inter - x1) / (x2 - x1)
    
    # Convertir de vuelta a escala original
    y_inter <- exp(log_y_inter)
    
    return(y_inter)
  }
  
  # 4. APLICAR INTERPOLACIÓN A LOS GAPS
  poblacion_completa <- poblacion_raw
  
  for (anio_faltante in anios_faltantes) {
    # Encontrar los puntos de referencia más cercanos
    anios_menores <- poblacion_raw$anio[poblacion_raw$anio < anio_faltante]
    anios_mayores <- poblacion_raw$anio[poblacion_raw$anio > anio_faltante]
    
    if (length(anios_menores) > 0 && length(anios_mayores) > 0) {
      # Punto anterior más cercano
      x1 <- max(anios_menores)
      y1 <- poblacion_raw$poblacion[poblacion_raw$anio == x1]
      
      # Punto posterior más cercano
      x2 <- min(anios_mayores)
      y2 <- poblacion_raw$poblacion[poblacion_raw$anio == x2]
      
      # Aplicar interpolación geométrica (más realista para población)
      valor_interpolado <- interpolacion_geometrica(x1, y1, x2, y2, anio_faltante)
      
      # Agregar a la base de datos
      nueva_fila <- data.frame(
        anio = anio_faltante,
        poblacion = round(valor_interpolado),
        fuente = "Interpolación geométrica"
      )
      
      poblacion_completa <- rbind(poblacion_completa, nueva_fila)
    }
  }
  
  # Ordenar por año
  poblacion_completa <- poblacion_completa %>% arrange(anio)
  poblacion_completa <- poblacion_completa %>% 
    select(anio,poblacion)
  return(poblacion_completa)
}
