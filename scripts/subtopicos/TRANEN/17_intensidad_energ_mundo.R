################################################################################
##                              Dataset:                                ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#' 
#'
limpiar_temps()

output_name <- "intensidad_energ_mundo"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R79C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  select(anio,iso3 = entities_code, valor_en_kwh_por_dolar = valor )


data <- data %>% 
  filter(!is.na(iso3))

df_output <- data

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso



comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  entrega_subtopico = "datasets_update",
  pk = c("anio", "iso3"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R79C0"),
    analista = "",
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_kwh_por_dolar" = "Consumo energético por unidad de PIB (en dólares PPA 2011)"),
    unidades = list("valor_en_kwh_por_dolar" = "kilowatt-hours per $ 2011 PPA")
  )

rm(list = ls())