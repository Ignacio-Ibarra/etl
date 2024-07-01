################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_sector_global_2016"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

emisiones_globales_2016_sector<-readr::read_csv(argendataR::get_temp_path("R125C51"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- emisiones_globales_2016_sector

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  emisiones_globales_2016_sector,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("sector","subsector","subsubsector"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R125C51"),
    analista = "",
    control = comparacion,
    pk = c("sector","subsector","subsubsector"),
    es_serie_tiempo = F,
    #columna_indice_tiempo = "",
    #columna_geo_referencia = "",
    nivel_agregacion = "global",
    aclaraciones = "ver como definir sector, subsector, subsubsector",
    etiquetas_indicadores = list("valor_en_porcent"="valor en porcentaje de las emisiones por sector"),
    unidades = list("valor_en_porcent" = "porcentaje")
  )

