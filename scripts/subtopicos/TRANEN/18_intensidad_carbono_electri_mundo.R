################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

limpiar_temps()

output_name <- "intensidad_carbono_electri_mundo"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_raw_path("R80C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----



data <- data %>% 
  mutate(entities_code = ifelse(str_detect(entities_name, "Latin America and Caribbean"), "LCN", entities_code))

data <- data %>% 
  select(anio,iso3 = entities_code, valor_en_gco2_por_kwh = valor )


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
    fuentes = c("R80C0"),
    analista = "",
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_gco2_por_kwh" = "Intensidad de carbono de la electricidad (por kWh)"),
    unidades = list("valor_en_gco2_por_kwh" = "grams of CO₂ equivalents per kilowatt-hour")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "TRANEN", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "TRANEN",  branch = "dev")
