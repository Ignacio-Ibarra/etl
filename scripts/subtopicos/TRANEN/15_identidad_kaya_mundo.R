################################################################################
##                              Dataset: identidad_kaya_mundo                 ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
limpiar_temps()
rm(list = ls())

output_name <- "identidad_kaya_mundo"
subtopico <- "TRANEN"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_raw_path("R78C0"))


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
         poblacion = population_historical,
         emision_anual_kgco2_por_kwh =annual_co2_emissions_per_unit_energy_kg_per_kilowatt_hour,
         emision_anual_kgco2_por_usd_ppa_2011 = annual_co2_emissions_per_gdp_kg_per_international,
         emision_anual_co2_ton  =annual_co2_emissions
  )


df_output <- data 



# corregir <- unique(df_output$iso3)[!unique(df_output$iso3) %>% check_iso3()]

df_output <- df_output %>% 
  mutate(iso3 = case_when(
    iso3 == "OWID_WRL" ~ "WLD",
    # iso3 == "OWID_KOS" ~ "XKX",
    T ~ iso3
  )) %>% 
  rename(geocodigoFundar = iso3)

geonomen <- get_nomenclador_geografico_front() 

geonomen <- geonomen %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- left_join(df_output, geonomen)

df_output <- df_output %>% 
  filter(!is.na(geonombreFundar))

check_iso3(df_output$geocodigoFundar)
#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(output_name, subtopico = subtopico)

df_anterior <- df_anterior %>% 
  filter(geocodigoFundar %in% c("WLD", "ARG"))

comparacion <- argendataR::comparar_outputs(
  df_output %>% 
    filter(geocodigoFundar %in% c("WLD", "ARG")),
  nombre = output_name,
  subtopico = "TRANEN",
  # entrega_subtopico = "datasets_update",
  pk = c("anio", "geocodigoFundar"),
  drop_joined_df = F
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
    control = comparacion,
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("geocodigoFundar" = "Codigo geografico",
                                 "geonombreFundar" = "Nombre geografico",
                                 "emision_anual_kgco2_por_usd_ppa_2011" = "Emisión anual Kg CO2 por USD PPA 2011",
                                 "energia_por_unidad_pib_kwh" = "Intensidad energética (por unidad de PIB medido en dólares de 2011 PPA)",
                                 "pib_per_cap_usd_ppa_2011" = "PIB per cápita en dólares PPA 2011",
                                 "poblacion" = "Población",
                                 "emision_anual_kgco2_por_kwh" = "Intensidad de carbono (CO2/kWh)",
                                 "emision_anual_co2_ton" = "Emisiones anuales de CO2"
                                 ),
    unidades = list("energia_por_unidad_pib_kwh" = "kilowatt-horas per $ 2011 PPA",
                    "pib_per_cap_usd_ppa_2011" = "$ 2011 PPA",
                    "emision_anual_kgco2_por_usd_ppa_2011" = "kilogramos CO2 per $ 2011 PPA",
                    "poblacion" = "personas",
                    "emision_anual_kgco2_por_kwh" = "kilogramos per kilowatt-hour",
                    "emision_anual_co2_ton" = "toneladas"
    )
  )



mandar_data(paste0(gsub("\\.csv$", "", output_name), ".csv"), subtopico = "TRANEN", branch = "main")
mandar_data(paste0(gsub("\\.csv$", "", output_name), ".json"), subtopico = "TRANEN",  branch = "main")



rm(list = ls())
