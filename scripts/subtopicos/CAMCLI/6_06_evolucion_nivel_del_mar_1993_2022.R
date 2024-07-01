#################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

output_name <- "06_evolucion_nivel_del_mar_1993_2022"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# traigo la data clean para este dataset (3 fuentes)

evol_nivel_mar<- readr::read_csv(argendataR::get_temp_path("R162C72"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df <- evol_nivel_mar

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- df
names(df_output)
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
    output_name = "evolucion_nivel_del_mar_1993_2022",
    subtopico = "CAMCLI",
    fuentes = c("R162C72"),
    analista = "",
    control = comparacion,
    pk = c("fecha"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "global",
    etiquetas_indicadores = list("fecha" = "Fecha","altura_nivel_mar_corr_tpac_drift" = "Altura del nivel del mar en metros","altura_nivel_mar_filtrada_corr_tpac_drift" = "Altura del nivel del mar filtrada en metros"),
    unidades = list("altura_nivel_mar_corr_tpac_drift" = "metros","altura_nivel_mar_filtrada_corr_tpac_drift"="metros")
  )

