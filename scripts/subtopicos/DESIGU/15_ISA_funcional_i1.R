################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "nombre del archivo de salida "

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R"
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
readr::read_csv(argendataR::get_temp_path("RXXCX"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- proceso

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("var1", "var2"), # variables pk del dataset para hacer el join entre bases
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

