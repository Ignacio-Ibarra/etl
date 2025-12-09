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
rm(list = ls())

output_name <- "potencia_instalada_renov_regional"
subtopico <- "TRANEN"

df_anterior <- descargar_output(output_name, subtopico)
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:


data <- read_fuente_clean("R467C304")


colnames(data)
data %>% distinct(ano, mes)

data %>% distinct(tecnologia, tipo_maquina )

data %>% distinct(region)

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  select(region, tecnologia, potencia_instalada_mw)

data <- data %>% 
  mutate(region = str_to_title(region)) %>% 
  mutate(region = case_when(
  str_detect(region, "Buenos Aires") ~ "CABA y Provincia de Buenos Aires",
  str_detect(region, "Gran") ~ "CABA y Provincia de Buenos Aires",
  str_detect(region, "Noreste") ~ "Noreste Argentino",
  str_detect(region, "Noroeste") ~ "Noroeste Argentino",
  str_detect(region, "Pata") ~ "Patagonia",
  T ~ region
  ))


# unique(df_anterior$region)[!unique(df_anterior$region) %in% unique(data$region)]

data <- data %>%
  filter(
    tecnologia %in% c(
      "Biogas",
      "Biomasa",
      # "Ciclos Combinados",
      "Eólica",
      # "Hidráulica",
      "Hidráulica renovable",
      # "Motor Diesel",
      # "Nuclear",
      "Solar"
      # "Turbina a gas",
      # "Turbovapor"
    )
  ) %>% 
  mutate(tecnologia = case_when(
    str_detect(tecnologia, "renovable|Hidr") ~ "Hidro",
    str_detect(tecnologia, "Bio") ~ "Bioenergía",
    str_detect(tecnologia, "Solar") ~ "Fotovoltaica",
    T ~ tecnologia
    
  ))
  
data <- data %>% 
  summarise(valor_en_mw = sum(potencia_instalada_mw),
            .by = c(region, tecnologia))

total <- data %>%
  summarise(valor_en_mw = sum(valor_en_mw, na.rm = T), .by = tecnologia) %>% 
  mutate(region = "Total")


data <- bind_rows(data, total)  


data <- data %>% 
  group_by(region) %>% 
  mutate(porcentaje = 100*valor_en_mw/sum(valor_en_mw, na.rm = T)) %>% 
  ungroup()


df_output <- data %>% 
  rename(tipo_energia = tecnologia) %>% 
  complete(region, tipo_energia, fill = list(valor_en_mw = 0, porcentaje = 0))



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  # entrega_subtopico = "datasets_update",
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
    control = comparacion,
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


mandar_data(paste0(gsub("\\.csv$", "", output_name), ".csv"), subtopico = "TRANEN", branch = "main")
mandar_data(paste0(gsub("\\.csv$", "", output_name), ".json"), subtopico = "TRANEN",  branch = "main")



rm(list = ls())
