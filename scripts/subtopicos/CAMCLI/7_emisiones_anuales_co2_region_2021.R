################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Emisiones anuales de co2 por región pporcentaje de cada país sobre world de owid
#'

output_name <- "emisiones_anuales_co2_region_2021"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

emis_anua_co2_reg_2021<- readr::read_csv(get_raw_path("R119C0"))
geonomenclador <- argendataR::get_nomenclador_geografico()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

# dejo la variables que ncesitamos
emis_anua_co2_reg_2021 <- emis_anua_co2_reg_2021 %>% 
  select(-name,-id,-unit,-shortUnit,-datasetId,-entities_id)

## transformo los datos
emis_anua_co2_reg_2021_long <- pivot_longer(emis_anua_co2_reg_2021, 
               cols = -c(entities_name,entities_code),  # Columnas a mantener fijas
               names_to = "anio",             # Nombre para la columna de años
               values_to = "valor")          # Nombre para la columna de valores

emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
  mutate(anio = parse_number(anio)) %>%  # Convierte la columna "anios" en numérica
  slice_max(order_by = anio) 

# armo valor referencia worl
valor_referencia <- emis_anua_co2_reg_2021_long %>%
  filter(entities_code == "OWID_WRL") %>%
  select(valor) %>%
  pull()

#  doy formato a porcent
emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
  mutate(
    valor_en_porcent = as.numeric(sprintf("%.9f", (valor / valor_referencia)))
  )

# final
emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
  inner_join(geonomenclador, by = c("entities_code" = "codigo_fundar")) %>% 
  rename(
    iso3 = entities_code,
    continente_fundar = continente_fundar,
    iso3_desc_fundar = desc_fundar,
    anio = anio,
    valor_en_porcent = valor_en_porcent
  ) %>%  
  select(iso3, continente_fundar, iso3_desc_fundar, anio, valor_en_porcent)

# elimino na
emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
  filter(!is.na(valor_en_porcent))

emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
  filter(!is.na(continente_fundar))


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- emis_anua_co2_reg_2021_long

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")

comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            pk = c("iso3"),
                                            drop_joined_df = F)


check_iso3(df_output$iso3)

df_output$iso3[df_output$iso3 == "OWID_KOS"] <- "XKX"

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R119C0"),
    analista = "",
    control = comparacion,
    pk = c("iso3","anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #    columna_geo_referencia = "",
    #    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor_en_porcent" = "Porcentaje emisiones anuales co2"),
    unidades = list("valor_en_porcent" = "porecentaje %"))

mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")

