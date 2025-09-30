################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Evoluciión de las anomalías temperatura mar y tierra
#'
rm(list = ls())
branch <- "main"
topico <- "CAMCLI"
output_name <- "evolucion_temperatura_base_1861_1890"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# levanto mar 
evol_temp_mar_1850_2023<- read_fuente_clean(292)

# levanto tierra 
evol_temp_tierra_1850_2023<- read_fuente_clean(293)

# transformo tierra

evol_temp_tierra_1850_2023 <- evol_temp_tierra_1850_2023 %>%
  mutate(
    fecha = as.Date(paste(year, month, "01", sep = "-"))
  ) %>%
  # Filtrar el rango de fechas
  filter(year >= 1850)

# Calcular el promedio de la variable anomalia_tierra solo para el período 1850 1880
coef_tierra <- evol_temp_tierra_1850_2023 %>%
  filter(between(fecha, as.Date('1861-01-01'), as.Date('1890-12-01'))) %>%
  summarise (promedio_anomalia = mean(monthly_anomaly)) %>% 
  select(promedio_anomalia) %>%
  pull()

# Restar el promedio de la variable anomalia_tierra a cada valor
evol_temp_tierra_1850_2023 <- evol_temp_tierra_1850_2023 %>%
  mutate(anomalia_corregida = monthly_anomaly - coef_tierra) %>%
  rename(
    anomalia_temperatura_tierra_relativ = anomalia_corregida) 

#anomalia_temperatura_tierra_relativ
#anomalia_temperatura_mar_relativ

# transformo mar

evol_temp_mar_1850_2023 <- evol_temp_mar_1850_2023 %>% 
  # selecciono variables
    # select(1,2,3) %>% 
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
  filter(anio >= 1850)

# Calcular el promedio de la variable anomalia_mar solo para el período 1850 1880
coef_mar <- evol_temp_mar_1850_2023 %>%
  filter(between(fecha, as.Date('1861-01-01'), as.Date('1890-12-01'))) %>%
  summarise (promedio_anomalia = mean(anomalia_mar)) %>% 
  select(promedio_anomalia) %>%
  pull()

# Restar el promedio de la variable anomalia_tierra a cada valor
evol_temp_mar_1850_2023 <- evol_temp_mar_1850_2023 %>%
  mutate(anomalia_corregida = anomalia_mar - coef_mar) %>%
  # select(4,3,5) %>% 
  rename(
    anomalia_temperatura_mar_relativ = anomalia_corregida) 


evol_temp_mar_1850_2023 <- evol_temp_mar_1850_2023 %>% 
  select(anio, mes, fecha, anomalia_temperatura_mar_relativ)

evol_temp_tierra_1850_2023 <- evol_temp_tierra_1850_2023 %>% 
  select(year, month, fecha, anomalia_temperatura_tierra_relativ)

## junto ambas bases

evol_temp_mar_tierra_1850_2023 <- inner_join(evol_temp_mar_1850_2023, evol_temp_tierra_1850_2023 %>% 
                                               select(-c(year, month)), by = "fecha") 


evol_temp_mar_tierra_1850_2023 <- evol_temp_mar_tierra_1850_2023 %>% 
  summarise(anomalia_temperatura_mar_relativ = mean(anomalia_temperatura_mar_relativ),
            anomalia_temperatura_tierra_relativ = mean(anomalia_temperatura_tierra_relativ),
            .by = "anio")


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso



df_anterior <- descargar_output(nombre= output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- evol_temp_mar_tierra_1850_2023

comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("fecha"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  pivot_
  # select(-c(anio, mes)) %>% 
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list("nombre_anterior" = "evolucion_temperatura_1850_2023",
                                "nombre_nuevo" = "evolucion_temperatura_base_1861_1890"),
    control = list("Cambio la linea base de comparacion. Dataset a nivel anual"),
    subtopico = "CAMCLI",
    fuentes = c("R121C292", "R122C293"),
    analista = "",
    pk = c("anio"),
    aclaraciones = "Anomalías de temperatura de la superficie de la tierra y la superficie del mar relativas al promedio de 1861 a 1890",
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "global",
    etiquetas_indicadores = list("anio" = "Año","anomalia_temperatura_tierra_relativ"="Anomalias de temperatura de la superficie de la tierra","anomalia_temperatura_mar_relativ"="Anomalias de temperatura de la superficie del mar"),
    unidades = list("anomalia_temperatura_mar_relativ" = "Grados C°","anomalia_temperatura_tierra_relativ" = "Grados C°")
  )


subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.json"),
                   path_remoto = glue::glue("{topico}/{output_name}.json"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)

subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.csv"),
                   path_remoto = glue::glue("{topico}/{output_name}.csv"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)

