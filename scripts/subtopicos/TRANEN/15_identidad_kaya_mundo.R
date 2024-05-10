################################################################################
##                              Dataset: identidad_kaya_mundo                 ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
limpiar_temps()

output_name <- "identidad_kaya_mundo"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R78C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  filter(!is.na(entities_code)) %>% 
  select(anio, name, entities_code, valor) 

data <- data %>% 
  pivot_wider(id_cols = c(anio, entities_code),
              names_from = name, values_from = valor) %>% 
  janitor::clean_names()


data <- data %>% 
  rename(iso3 = entities_code,
         energia_por_unidad_pib_kwh = primary_energy_consumption_per_gdp_k_wh,
         pib_per_cap_usd_ppa_2011 = gdp_per_capita, 
         poblacion = population_historical_estimates,
         emision_anual_kgco2_por_kwh =annual_co2_emissions_per_unit_energy_kg_per_kilowatt_hour,
         emision_anual_kgco2_por_usd_ppa_2011 = annual_co2_emissions_per_gdp_kg_per_international,
         emision_anual_co2_ton  =annual_co2_emissions
  )


df_output <- data %>% 
  select(-emision_anual_kgco2_por_usd_ppa_2011)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  entrega_subtopico = "datasets_segunda_entrega",
  pk = c("anio", "iso3"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R78C0"),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("energia_por_unidad_pib_kwh" = "Intensidad energética (por unidad de PIB medido en dólares de 2011 PPA)",
                                 "pib_per_cap_usd_ppa_2011" = "PIB per cápita en dólares PPA 2011",
                                 "poblacion" = "Población",
                                 "emision_anual_kgco2_por_kwh" = "Intensidad de carbono (CO2/kWh)",
                                 "emision_anual_co2_ton" = "Emisiones anuales de CO2"
                                 ),
    unidades = list("energia_por_unidad_pib_kwh" = "kilowatt-hours per $ 2011 PPA",
                    "pib_per_cap_usd_ppa_2011" = "$ 2011 PPA",
                    "poblacion" = "persons",
                    "emision_anual_kgco2_por_kwh" = "kilograms per kilowatt-hour",
                    "emision_anual_co2_ton" = "tonnes"
    )
  )

rm(list = ls())
