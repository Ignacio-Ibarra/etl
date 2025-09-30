################################################################################
##                              Dataset: nombre                               ##
################################################################################



#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

rm(list = ls())

branch <- "main"
topico <- "CAMCLI"
output_name <- "evolucion_temperatura_aire"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

evol_temp_aire_1850_2020<- read_fuente_clean("R450C291") %>% 
  janitor::clean_names()

#-- Parametros Generales ----
# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

argendataR::check_iso3(evol_temp_aire_1850_2020$code)


evol_temp_aire_1850_2020 <- evol_temp_aire_1850_2020 %>% 
  filter(!is.na(code)) %>% 
  mutate(code = ifelse(code  == "OWID_WRL", "WLD", code))

argendataR::check_iso3(evol_temp_aire_1850_2020$code)


df_output <- evol_temp_aire_1850_2020 %>% 
  select(-entity) %>% 
  left_join(get_nomenclador_geografico_front(), by = c("code" = "geocodigo")) %>% 
  rename(geocodigoFundar = code, geonombreFundar = name_long) %>% 
  select(-c(name_short, near_surface_temperature_anomaly_lower, near_surface_temperature_anomaly_upper, iso_2))

# comparacion <- argendataR::comparar_outputs(
#   df_output,
#   subtopico = "CAMCLI",
#   nombre = output_name,
#   k_control_num = 3,
#   pk = c("geocodigoFundar", "year"),
#   drop_joined_df = F
# )

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

#head(evol_temp_aire_1850_2020)
df_output %>%
  argendataR::write_output(
    cambio_nombre_output = list("nombre_nuevo" = "evolucion_temperatura_aire", 
                                "nombre_anterior" = "evolucion_temperatura_aire_1850_2020"),
    output_name = output_name,
    control = list("Cambio de fuente raw de R112C0 a R450C0 porque la fuente anterior no se acutaliza mas."),
    subtopico = "CAMCLI",
    fuentes = c("R450C291"),
    analista = "",
    pk = c("geocodigoFundar", "year"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "year",
    columna_geo_referencia = "geocodigoFundar", 
    #    nivel_agregacion = "pais",   
    descripcion_columnas = list("near_surface_temperature_anomaly" = "Anomalía temperatura  anual respecto al promedio de 1861 - 1890",
                                 "geocodigoFundar" = "Geocodigo de nomenclador de argendata", 
                                 "year" = "Año", "geonombreFundar" = "Name long de nomenclador de argendata"),
    unidades = list("near_surface_temperature_anomaly" = "°C"))

subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.json"),
                   path_remoto = glue::glue("{topico}/{output_name}.json"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)

subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.csv"),
                   path_remoto = glue::glue("{topico}/{output_name}.csv"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)
