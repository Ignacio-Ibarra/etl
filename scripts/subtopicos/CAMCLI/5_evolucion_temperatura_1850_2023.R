################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Evoluciión de las anomalías temperatura mar y tierra
#'

output_name <- "evolucion_temperatura_1850_2023"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# levanto mar 
evol_temp_mar_1850_2023<-readr::read_csv(argendataR::get_temp_path("R121C0"))

# levanto tierra 
evol_temp_tierra_1850_2023<-fread(argendataR::get_temp_path("R122C0"),skip = 35)

# transformo tierra

evol_temp_tierra_1850_2023 <- evol_temp_tierra_1850_2023 %>%
  # selecciono variables
  select(1,2,5) %>% 
  # Cambiar el nombre de las variables
  rename(
    anio = V1,
    mes = V2,
    anomalia_tierra = V5
  ) %>%
  # Crear la variable fecha
  mutate(
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  ) %>%
  # Filtrar el rango de fechas
  filter(between(fecha, as.Date('1850-01-01'), as.Date('2023-12-01')))

# Calcular el promedio de la variable anomalia_tierra solo para el período 1850 1880
coef_tierra <- evol_temp_tierra_1850_2023 %>%
  filter(between(fecha, as.Date('1850-01-01'), as.Date('1880-12-01'))) %>%
  summarise (promedio_anomalia = mean(anomalia_tierra)) %>% 
  select(promedio_anomalia) %>%
  pull()

# Restar el promedio de la variable anomalia_tierra a cada valor
evol_temp_tierra_1850_2023 <- evol_temp_tierra_1850_2023 %>%
  mutate(anomalia_corregida = anomalia_tierra - coef_tierra) %>%
  select(4,3,5) %>% 
  rename(
    anomalia_temperatura_tierra_relativ = anomalia_corregida) 

#anomalia_temperatura_tierra_relativ
#anomalia_temperatura_mar_relativ

# transformo mar

evol_temp_mar_1850_2023 <- evol_temp_mar_1850_2023 %>% 
  # selecciono variables
    select(1,2,3) %>% 
  # renombre variables
  rename(
    anio = year,
    mes = month,
    anomalia_mar = anomaly
  ) %>%
  mutate(
    fecha = as.Date(paste(anio, mes, "01", sep = "-"))
  ) %>%
  # Filtrar el rango de fechas
  filter(between(fecha, as.Date('1850-01-01'), as.Date('2023-12-01')))

# Calcular el promedio de la variable anomalia_mar solo para el período 1850 1880
coef_mar <- evol_temp_mar_1850_2023 %>%
  filter(between(fecha, as.Date('1850-01-01'), as.Date('1880-12-01'))) %>%
  summarise (promedio_anomalia = mean(anomalia_mar)) %>% 
  select(promedio_anomalia) %>%
  pull()

# Restar el promedio de la variable anomalia_tierra a cada valor
evol_temp_mar_1850_2023 <- evol_temp_mar_1850_2023 %>%
  mutate(anomalia_corregida = anomalia_mar - coef_mar) %>%
  select(4,3,5) %>% 
  rename(
    anomalia_temperatura_mar_relativ = anomalia_corregida) 

## junto ambas bases

evol_temp_mar_tierra_1850_2023 <- inner_join(evol_temp_mar_1850_2023, evol_temp_tierra_1850_2023, by = "fecha") %>%
  select(1,5,3)

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_outoput <- evol_temp_mar_tierra_1850_2023

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  evol_temp_mar_tierra_1850_2023,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("fecha"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R121C0", "R122C0"),
    analista = "",
    pk = c("fecha"),
    es_serie_tiempo = T,
    aclaraciones = "cambió mucho este output respecto al dataset original del analaista porque se hicieron modificaciones en función de cálculos (gráfico anomalias tem mar tierra - risaro)",
    columna_indice_tiempo = "fecha",
    #columna_geo_referencia = "iso3",
    #nivel_agregacion = "pais",
    etiquetas_indicadores = list("anomalia_temperatura_mar_relativ" = "Anomalía temperatura mar realtiva respecto a promedio temperatura período 1850-1880",
                                 "anomalia_temperatura_tierra_relativ" = "Anomalía temperatura tierra realtiva respecto a promedio temperatura período 1850-1880"),
    unidades = list("anomalia_temperatura_tierra_relativ" = "°C","anomalia_temperatura_mar_relativ" = "°C"),
    directorio = "data/CAMCLI/"
)

