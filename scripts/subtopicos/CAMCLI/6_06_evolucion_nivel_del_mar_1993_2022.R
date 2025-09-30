#################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

rm(list = ls())
branch <- "main"
topico <- "CAMCLI"

output_name <- "06_evolucion_nivel_del_mar"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# traigo la data clean para este dataset (3 fuentes)

evol_nivel_mar<- read_fuente_clean(294)
  
  
#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- evol_nivel_mar %>% 
  select(code, day, sea_level_average) %>% 
  mutate(sea_level_average = sea_level_average-mean(evol_nivel_mar$sea_level_average[year(evol_nivel_mar$day) %in% 1880:1900])) %>% 
  mutate(sea_level_average = sea_level_average/10, 
         anio = year(day)) %>%
  summarise(sea_level_average = mean(sea_level_average), .by = c(anio, code))

df_output$code <- "WLD"

df_output <- df_output %>% 
  rename("geocodigoFundar" = code)

check_iso3(df_output$geocodigoFundar)

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(df,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("fecha"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list("nombre_anterior" = "06_evolucion_nivel_del_mar_1993_2022", 
                                "nombre_nuevo" = output_name),
    control = list("Cambio de dataset fuente y baseline."),
    subtopico = "CAMCLI",
    fuentes = c("R451C294"),
    aclaraciones = "NOAA Climate.gov - Climate Change: Global Sea Level (2022).
Data from Philip Thompson, University of Hawaii Sea Level Center, based on various sources:
* The early part of the time series comes from the sea level group of CSIRO (Commonwealth Scientific and Industrial Research Organisation), Australia's national science agency. Church, J. A., and White, N. J. (2011). Sea-Level Rise from the Late 19th to the Early 21st Century. Surveys in Geophysics, 32(4-5), 585-602. http://doi.org/10.1007/s10712-011-9119-1
* The more recent part of the time series is from the University of Hawaii Sea Level Center (UHSLC). It is based on a weighted average of 373 global tide gauge records collected by the U.S. National Ocean Service, UHSLC, and partner agencies worldwide.",
    analista = "",
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "global",
    etiquetas_indicadores = list("anio" = "Año","sea_level_average" = "Diferencia del nivel del mar en cm relativa al promedio de 1880-1900",
                                 "geocodigoFundar" = "geocodigo"),
    unidades = list("sea_level_average" = "cm")
  )


subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.json"),
                   path_remoto = glue::glue("{topico}/{output_name}.json"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)

subir_o_actualizar(path_local = glue::glue("{tempdir()}/{output_name}.csv"),
                   path_remoto = glue::glue("{topico}/{output_name}.csv"),
                   repo_owner = "argendatafundar", repo_name = "data", branch = branch)


