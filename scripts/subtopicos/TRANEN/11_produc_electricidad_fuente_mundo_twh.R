################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
limpiar_temps()

output_name <- "produc_electricidad_fuente_mundo_twh"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R77C0"))


data <- data %>% 
  filter(!is.na(entities_code))

data <- data %>% 
  mutate(tipo_energia = case_when(
    name == "Other renewables excluding bioenergy - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Otras renovables",
    name == "Electricity from bioenergy - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Bioenergia",
    name == "Electricity from solar - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Solar",
    name == "Electricity from coal - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Carbon",
    name == "Electricity from gas - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Gas natural",
    name == "Electricity from oil - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Petroleo",
    name == "Electricity from wind - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Eolica",
    name == "Electricity from nuclear - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Nuclear",
    name == "Electricity from hydro - TWh (adapted for visualization of chart electricity-prod-source-stacked)" ~ "Hidro"
  )) %>% 
  rename(valor_en_twh = valor,
         iso3 = entities_code)

total <- data %>% 
  group_by(anio, iso3) %>% 
  summarise(tipo_energia = "Total",
            valor_en_twh = sum(valor_en_twh, na.rm = T)) %>%
  ungroup()

data <- data %>% 
  select(anio, iso3, tipo_energia, valor_en_twh) %>% 
  bind_rows(total)

data <- data %>% 
  group_by(anio, iso3) %>% 
  mutate(porcentaje = 100*valor_en_twh/valor_en_twh[tipo_energia == "Total"]) %>% 
  ungroup()

data <- data %>% 
  mutate(porcentaje = replace_na(porcentaje, 0))

# data <- data %>% 
#   mutate(tipo_energia = case_when(
#     fuente_energia %in% c("Gas natural", "Carbon", "Petroleo") ~ "Sucias",
#     fuente_energia == "Total" ~ "Total",
#     T ~ "Limpias"
#   ))
# 
# data <- data %>% 
#   group_by(iso3) %>% 
#   complete(anio, fuente_energia = "Biocombustibles", tipo_energia = "Limpias" ) %>% 
#   mutate(porcentaje = replace_na(porcentaje, 0)) %>% 
#   ungroup()

df_output <- data


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  entrega_subtopico = "datasets_update",
  pk = c("anio", "iso3", "tipo_energia"),
  drop_joined_df = F
)




#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R77C0"),
    analista = "",
    pk = c("anio", "iso3", "tipo_energia"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("tipo_energia" = "Tipo de energía",
                                 "valor_en_twh" = "Producción de energía en Terawatts hora"),
    unidades = list("valor_en_twh" = "TWh")
  )

rm(list = ls())
