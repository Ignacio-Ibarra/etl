################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_sector_global_2016"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

emisiones_globales_2016_sector<- read_fuente_clean("R125C51")

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- emisiones_globales_2016_sector

df_anterior <- descargar_output(nombre = output_name, 
                                subtopico = "CAMCLI", 
                                entrega_subtopico = "datasets_segunda_entrega")


comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  pk = c("sector","subsector","subsubsector"),
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
    fuentes = c("R125C51"),
    analista = "",
    pk = c("sector","subsector","subsubsector"),
    es_serie_tiempo = F,
    #columna_indice_tiempo = "",
    #columna_geo_referencia = "",
    nivel_agregacion = "global",
    aclaraciones = "Sin cambios. Datos a 2016",
    etiquetas_indicadores = list("valor_en_porcent"="valor en porcentaje de las emisiones por sector"),
    unidades = list("valor_en_porcent" = "porcentaje")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")


