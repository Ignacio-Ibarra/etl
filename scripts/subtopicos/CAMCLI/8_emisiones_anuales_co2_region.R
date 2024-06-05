################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_anuales_co2_region"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
emis_anual_co2_region<-readr::read_csv(argendataR::get_temp_path("R119C0"))
geonomenclador <- argendataR::get_nomenclador_geografico()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Procesamiento ----

## transformo los datos
emis_anual_co2_region <- emis_anual_co2_region %>% 
  select (2,7,8,81) %>% 
  rename(anio = "2021")

# armo valor referencia worl
valor_referencia <- evol_anua_co2_reg_2021 %>%
  filter(entities_code == "OWID_WRL") %>%
  select(anio) %>%
  pull()

#  doy formato a porcent
evol_anua_co2_reg_2021 <- evol_anua_co2_reg_2021 %>%
  mutate(valor_en_porcent = sprintf("%.9f", (anio / valor_referencia)))

# final
evol_anua_co2_reg_2021 <- evol_anua_co2_reg_2021 %>%
  inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar")) %>% 
  mutate(anio = 2021)  %>%  
  select(3,10,8,4,5)  %>% 
  rename(
    iso3 = entities_code,
    continente_fundar = continente_fundar,
    iso3_desc_fundar = desc_fundar,
    anio = anio,
    valor_en_porcent = valor_en_porcent
  ) %>% 
  mutate(valor_en_porcent = as.numeric(valor_en_porcent))

view(evol_anua_co2_reg_2021)

# elimino na
evol_anua_co2_reg_2021 <- na.omit(evol_anua_co2_reg_2021)

df_output <- evol_anua_co2_reg_2021


df_outoput <- proceso

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "iso3"),
  drop_output_drive = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R37C1", "R34C2"),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("pbi_per_capita_ppa_porcentaje_argentina" = "PBI per cápita PPA como porcentaje del de Argentina"),
    unidades = list("pbi_per_capita_ppa_porcentaje_argentina" = "porcentaje")
  )

