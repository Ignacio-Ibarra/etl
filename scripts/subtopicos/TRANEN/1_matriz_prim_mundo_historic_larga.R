################################################################################
##      Consumo energético por fuente a nivel global, 1800-2022. (En TWh)                               ##
################################################################################

#-- Descripcion ----
#' Consumo energético por fuente a nivel global, 1800-2022. (En TWh Y %)
#'
limpiar_temps()

output_name <- "matriz_prim_mundo_historic_larga"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R47C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  mutate(tipo_energia = case_when(
    name == "Other renewables (TWh, substituted energy)" ~ "Otras renovables",
    name == "Biofuels (TWh, substituted energy)" ~ "Biocombustibles",
    name == "Solar (TWh, substituted energy)" ~ "Solar",
    name == "Coal (TWh, substituted energy)" ~ "Carbón",
    name == "Gas (TWh, substituted energy)" ~ "Gas natural",
    name == "Oil (TWh, substituted energy)" ~ "Petróleo",
    name == "Wind (TWh, substituted energy)" ~ "Eólica",
    name == "Nuclear (TWh, substituted energy)" ~ "Nuclear",
    name == "Hydropower (TWh, substituted energy)" ~ "Hidro",
    name == "Traditional biomass (TWh, substituted energy)" ~ "Biomasa tradicional"
  )) %>% 
  rename(valor_en_twh = valor)

total <- data %>% 
  group_by(anio) %>% 
  summarise(tipo_energia = "Total",
            valor_en_twh = sum(valor_en_twh, na.rm = T))

data <- data %>% 
  select(anio, tipo_energia, valor_en_twh) %>% 
  bind_rows(total)

data <- data %>% 
  group_by(anio) %>% 
  mutate(porcentaje = 100*valor_en_twh/valor_en_twh[tipo_energia == "Total"])


df_output <- data

#-- Controlardata#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  entrega_subtopico = "datasets_segunda_entrega",
  subtopico = "TRANEN",
  pk = c("anio", "tipo_energia"),
  drop_output_drive = F
)

# fix caracteres rotos x encoding
comparacion$output_drive$tipo_energia <- comparacion$output_drive$tipo_energia %>% gsub("\xf3", "ó",.)

# comparacion manual
x <- left_join(comparacion$output_drive, df_output, by = c("anio", "tipo_energia"))
sum(is.na(x$valor_en_twh.y))
cor(x$valor_en_twh.x, x$valor_en_twh.y) > 0.99

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R47C0"),
    analista = "",
    pk = c("anio", "tipo_energia"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "mundo",
    etiquetas_indicadores = list("valor_en_twh" = "Energía en TWh", "tipo_energia" = "Tipo de energía",
                                 "porcentaje" = "Porcentaje del total anual"),
    unidades = list("valor_en_twh" = "TWh", "porcentaje" = "porcentaje")
  )

rm(list = ls())