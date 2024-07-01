################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_global_sec_1850_2014"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
emisiones_glob_sect_1850_2014 <- readr::read_csv(argendataR::get_temp_path("R132C56"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- emisiones_glob_sect_1850_2014

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  emisiones_glob_sect_1850_2014,
  subtopico = "CAMCLI",
  entrega_subtopico = "segunda_entrega",
  nombre = output_name,
  k_control_num = 3,
  pk = c("sector","anio"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    fuentes = c("R132C56"),
    analista = "",
    control = comparacion,
    pk = c("anio", "sector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "mundo",
    etiquetas_indicadores = list("sector" = "sector"),
    unidades = list("valor_en_ggco2e" = "Mil toneladas de CO2 equivalente")
  )

