################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_per_cap"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
emis_per_cap_co2_toneladas<-readr::read_csv(argendataR::get_temp_path("R123C0"))
geonomenclador <- argendataR::get_nomenclador_geografico()


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

# me quedo con las variables que necesitamos
emis_per_cap_co2_toneladas <- emis_per_cap_co2_toneladas %>% 
  select(-name,-id,-unit,-shortUnit,-datasetId,-entities_id)

# paso a formato long
emis_per_cap_co2_toneladas_long <- pivot_longer(emis_per_cap_co2_toneladas, 
                                           cols = -c(entities_name,entities_code),  # Columnas a mantener fijas
                                           names_to = "anio",             # Nombre para la columna de años
                                           values_to = "valor")          # Nombre para la columna de valores

# paso anios a numeric
emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>%
  mutate(anio = as.numeric(anio))

# traigo info geonomenclagor
emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>% 
  inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar"))

# dejo la variables que ncesitamos
emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>% 
  select(entities_code,anio,valor) %>% 
  rename(iso3 = entities_code,
         valor_en_ton=valor)

## elimino NA en valor
emis_per_cap_co2_toneladas_long <- emis_per_cap_co2_toneladas_long %>%
  filter(!is.na(valor_en_ton))

df_output <- emis_per_cap_co2_toneladas_long

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  df_output,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("iso3","anio"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R123C0"),
    analista = "",
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = "se agrega en la actualización anio 2022",
    etiquetas_indicadores = list("valor_en_ton" = "Valor en toneladas"),
    unidades = list("valor_en_ton" = "toneladas")
  )

