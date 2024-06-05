################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_anuales_co2_ch4_n20_1850_2022"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# cargo la data
emi_anua_co2_ch4_n20_1850_2022<-readr::read_csv(argendataR::get_temp_path("R114C0"))

# me quedo con las variables que necesitamos
emi_anua_co2_ch4_n20_1850_2022 <- emi_anua_co2_ch4_n20_1850_2022 %>% 
  select(6,8,9,10:181)

# paso a formato long

emi_anua_co2_ch4_n20_1850_2022 <- pivot_longer(emi_anua_co2_ch4_n20_1850_2022, 
                           cols = -c(title, entities_name,entities_code),  # Columnas a mantener fijas
                           names_to = "anios",             # Nombre para la columna de años
                           values_to = "valor")          # Nombre para la columna de valores

# transformo los datos para igualarlos al dataset emisiones_anuales_co2_ch4_n20_1850_2022.csv
emi_anua_co2_ch4_n20_1850_2022 <- emi_anua_co2_ch4_n20_1850_2022 %>%
    filter(entities_name=="World") %>% 
    group_by(anios,title) %>%
    summarise(valor_nuevo = sum(valor)) %>% 
    mutate(anios = as.Date(paste(anios, "-01-01", sep = ""), format = "%Y-%m-%d"))

# pivoteo widw y cambio nombres de variables  
emi_anua_co2_ch4_n20_1850_2022 <- pivot_wider(emi_anua_co2_ch4_n20_1850_2022, 
                          names_from = title, 
                          values_from = valor_nuevo) %>%
  rename(emisiones_anuales_co2_toneladas = "Annual CO₂ emissions",
         emisiones_anuales_ch4_en_co2_toneladas = "Annual methane emissions",
         emisiones_anuales_n2o_en_co2_toneladas = "Annual nitrous oxide emissions",
         fecha = "anios")

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- emi_anua_co2_ch4_n20_1850_2022

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  emi_anua_co2_ch4_n20_1850_2022,
  subtopico = "CAMCLI",
  nombre = output_name,
  entrega_subtopico = "datasets_segunda_entrega",
  pk = c("fecha"),
  k_control_num = 3,
  drop_joined_df  = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R114C0"),
    analista = "",
    pk = c("fecha"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha",
  # columna_geo_referencia = "iso3",
  # nivel_agregacion = "mundial",
    etiquetas_indicadores = list("emisiones_anuales_co2_toneladas" = "Emisiones CO2 en toneladas","emisiones_anuales_ch4_en_co2_toneladas" = "Emisiones CH4 en CO2 en toneladas",
                                 "emisiones_anuales_n2o_en_co2_toneladas" = "Emisiones N2O en CO2 en toneladas"),
    unidades = list("emisiones_anuales_co2_toneladas" = "toneladas", "emisiones_anuales_ch4_en_co2_toneladas"="toneladas","emisiones_anuales_n2o_en_co2_toneladas"="toneladas"),
    directorio = "data/CAMCLI/")
