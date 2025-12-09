################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Emisiones anuales de co2 por región pporcentaje de cada país sobre world de owid
#'

rm(list = ls())

output_name <- "emisiones_gei_region_participacion"

#-- Librerias ----
geonomenclador <- argendataR::get_nomenclador_geografico_front()

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# emis_anua_co2_reg_2021<- readr::read_csv(get_raw_path("R119C0"))
df <- readr::read_csv(get_raw_path("R468C0"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


df <- df %>%
  janitor::clean_names() %>% 
  rename(anio = year, valor  = annual_emissions_ghg_total_co2eq) %>% 
  slice_max(order_by = anio) 


# armo valor referencia worl
valor_referencia <- df %>%
  filter(code == "OWID_WRL") %>%
  select(valor) %>%
  pull()

# no incluye trasporte internacional
# emis_anua_co2_reg_2021_long <- df %>% 
#   mutate(entities_code = ifelse(entities_name %in% c("International shipping", "International aviation"), "TRANS", code))

# emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>% 
#   summarise(valor = sum(valor, na.rm = T), .by = c(entities_code, anio))

#  doy formato a porcent
df <- df %>%
  mutate(
    valor_en_porcent = as.numeric(sprintf("%.9f", (valor / valor_referencia)))
  )


check_iso3(df$code)

df$code[df$code == "OWID_WRL"] <- "WLD"


check_iso3(df$code)


# final
df <- df %>%
  inner_join(geonomenclador, by = c("code" = "geocodigo")) %>% 
  rename(
    geocodigoFundar = code,
    anio = anio,
    valor_en_porcent = valor_en_porcent
  ) %>%  
  select(geocodigoFundar, geonombreFundar = name_long,
         anio, valor_en_porcent)


# emis_anua_co2_reg_2021_long <- emis_anua_co2_reg_2021_long %>%
#   filter(!is.na(continente_fundar))


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- df

df_anterior <- descargar_output(nombre="emisiones_anuales_gei_region.csv",
                                subtopico = "CAMCLI")

comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            pk = c("geocodigoFundar"),
                                            drop_joined_df = F)

comparacion$comparacion_cols$anio <- NULL


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,cambio_nombre_output = list('nombre_nuevo' = output_name, 'nombre_anterior' = 'emisiones_anuales_gei_region'),
    subtopico = "CAMCLI",
    fuentes = c("R468C0"),
    analista = "",
    control = comparacion,
    pk = c("geocodigoFundar","anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #    columna_geo_referencia = "",
    #    nivel_agregacion = "pais",
    aclaraciones = "Cambio de versiones incomparables. El dataset ahora representa porcentaje de emisiones GEI por región sobre el total del mundo para el último año disponible",
    etiquetas_indicadores = list("geocodigoFundar" = "codigo de geonomenclador",
                                 "geonombreFundar" = "nombre geografico",
                                 'anio' = "Año",
                                 "valor_en_porcent" = "Porcentaje emisiones anuales co2"),
    unidades = list("valor_en_porcent" = "porecentaje %"))

mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")

