################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "matriz_prim_mundo_historic.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_temp_path("R48C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  filter(!is.na(entities_code))

data <- data %>% 
  mutate(fuente_energia = case_when(
    name == "Other renewables (including geothermal and biomass) - TWh" ~ "Otras renovables",
    name == "Biofuels consumption - TWh" ~ "Biocombustibles",
    name == "Solar consumption - TWh" ~ "Solar",
    name == "Coal consumption - TWh" ~ "Carbon",
    name == "Gas consumption - TWh" ~ "Gas natural",
    name == "Oil consumption - TWh" ~ "Petroleo",
    name == "Wind consumption - TWh" ~ "Eolica",
    name == "Nuclear consumption - TWh" ~ "Nuclear",
    name == "Hydro consumption - TWh" ~ "Hidro"
    )) %>% 
  rename(valor_en_twh = valor,
         iso3 = entities_code)

total <- data %>% 
  group_by(anio, iso3) %>% 
  summarise(fuente_energia = "Total",
            valor_en_twh = sum(valor_en_twh, na.rm = T)) %>%
  ungroup()

data <- data %>% 
  select(anio, iso3, fuente_energia, valor_en_twh) %>% 
  bind_rows(total)

data <- data %>% 
  group_by(anio, iso3) %>% 
  mutate(porcentaje = 100*valor_en_twh/valor_en_twh[fuente_energia == "Total"]) %>% 
  ungroup()

data <- data %>% 
  mutate(tipo_energia = case_when(
    fuente_energia %in% c("Gas natural", "Carbon", "Petroleo") ~ "Sucias",
    fuente_energia == "Total" ~ "Total",
    T ~ "Limpias"
  ))

df_output <- data

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  entrega_subtopico = "datasets_segunda_entrega",
  subtopico = "TRANEN",
  pk = c("anio", "iso3", "tipo_energia", "fuente_energia"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = c("R48C0"),
    analista = "",
    subtopico = "TRANEN",
    pk = c("anio", "iso3", "tipo_energia", "fuente_energia"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais y mundo",
    etiquetas_indicadores = list("tipo_energia" = "Tipo de energía",
                                 "fuente_energia" = "Fuente de energía",
                                 "valor_en_twh" = "Energía en TWh",
                                 "porcentaje" = "Porcentaje"),
    unidades = list("valor_en_twh" = "TWh", "porcentaje" = "porcentaje")
  )

