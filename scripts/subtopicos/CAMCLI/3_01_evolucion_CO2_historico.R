################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "01_evolucion_CO2_historico"

#-- Librerias ----

#-- Lectura de Datos ----
# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
co2_hist<-read_csv(argendataR::get_temp_path("R100C26"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Controlar Output ----

df_output <- co2_hist

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

comparacion <- argendataR::comparar_outputs(
  co2_hist,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("fecha_estimada"),
  drop_joined_df = F
)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R100C26"),
    analista = "",
    pk = c("fecha_estimada"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha_estimada",
#    columna_geo_referencia = "",
#    nivel_agregacion = "pais",
    etiquetas_indicadores = list("co2_ppm" = "Concentración co2 atmósfera"),
    unidades = list("co2_ppm" = "partes por millón (ppm)"),
    directorio = "data/CAMCLI/"
  )


list.files(tempdir())
