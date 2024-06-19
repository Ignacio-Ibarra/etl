################################################################################
##                              Dataset: potencia_instalada_renov_regional    ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#' Tomo "Región" como "region", "Eólica (MW)" como "Eólica", "Fotovoltaica (MW)" como "Fotovoltaica",
#' "Hidráulica" como "Hidro", "Bioenergías (MW)" como "Bioenergía", "Total (MW)" como "Total".
#' Replico los nombres de las energías y obtengo los porcentajes en "porcentaje". 
#' Paso a formato long, con los valores en MW en "valor_en_mw"
limpiar_temps()

output_name <- "potencia_instalada_renov_regional"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R81C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  rename(region = nemoRegion, tipo_energia = name) %>% 
  select(- unidad)

data$tipo_energia

data <- data %>%
  mutate(tipo_energia = case_when(
    str_detect(tipo_energia, "hidraulica_menorigual_50") ~ "Hidro",
    str_detect(tipo_energia, "fotovoltaica") ~ "Fotovoltaica",
    str_detect(tipo_energia, "biocom") ~ "Bioenergía",
    str_detect(tipo_energia, "eolica") ~ "Eólica"
  ))

data <- data %>%
  filter(!is.na(tipo_energia))

data <- data %>% 
  mutate(region = case_when(
    region == "NOA"      ~     "Noroeste Argentino",
    region == "NEA"      ~ "Noreste Argentino",
    region == "CUY"      ~ "Cuyo",               
    region == "CEN"      ~ "Centro",             
    region == "LIT"      ~ "Litoral",             
    region == "COM"      ~ "Comahue",
    region == "PAT"      ~ "Patagonia",                        
    region == "BAS + GBA"~ "CABA y Provincia de Buenos Aires",
    region == "Total" ~ "Total"
  ))

data <- data %>% 
  filter(!is.na(region))

data <- data %>% 
  group_by(anio, region, tipo_energia) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup()

total <- data %>%
  group_by(anio, tipo_energia) %>% 
  summarise(value = sum(value, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(region = "Total")

data <- bind_rows(data, total)

data <- data %>% 
  group_by(region, anio) %>% 
  mutate(porcentaje = 100*value/sum(value, na.rm = T)) %>% 
  ungroup()

data <- data %>% 
  rename(valor_en_mw = value) %>% 
  select(-anio)





df_output <- data



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  entrega_subtopico = "datasets_update",
  pk = c("region", "tipo_energia"),
  drop_joined_df = F
)

# comparacion$output_drive %>%
#   view()



#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R81C0"),
    analista = "",
    pk = c("region", "tipo_energia"),
    es_serie_tiempo = F,
    # columna_indice_tiempo = "anio",
    # columna_geo_referencia = "",
    nivel_agregacion = "region y pais",
    etiquetas_indicadores = list("region" = "Región",
                                 "tipo_energia" = "Tipo de energía renovable",
                                 "valor_en_mw" = "Potencia instalada MW",
                                 "porcentaje" = "Porcentaje sobre el total de capacidad instalada renovable"),
    unidades = list("valor_en_mw" = "MW", "porcentaje" = "Porcentaje")
  )

rm(list = ls())