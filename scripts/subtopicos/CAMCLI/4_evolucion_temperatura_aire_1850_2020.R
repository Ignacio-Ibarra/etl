################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "evolucion_temperatura_aire_1850_2020"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

evol_temp_aire_1850_2020<-readr::read_csv(argendataR::get_temp_path("R112C28"))

#-- Parametros Generales ----
# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- evol_temp_aire_1850_2020

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  df_output,
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

#head(evol_temp_aire_1850_2020)
df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R112C28"),
    analista = "",
    control = comparacion,
    pk = c("fecha"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha",
    #    columna_geo_referencia = "",
    #    nivel_agregacion = "pais",
    etiquetas_indicadores = list("anomalia_temperatura_deg_c" = "Anomalía temperatura aire en °C"),
    unidades = list("anomalia_temperatura_deg_c" = "°C"))

