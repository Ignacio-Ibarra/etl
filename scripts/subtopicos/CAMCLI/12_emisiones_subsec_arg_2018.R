################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_subsec_arg_2018"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
emisiones_sector_2018_argentina<-readr::read_csv(argendataR::get_temp_path("R131C55"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----
#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df <- emisiones_sector_2018_argentina

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

df_output <- df

comparacion <- argendataR::comparar_outputs(df,
  df_anterior,
  k_control_num = 3,
  pk = c("sector","anio","subsector"),
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
    fuentes = c("R131C55"),
    analista = "",
    control = comparacion,
    pk = c("sector","anio","subsector"),
    es_serie_tiempo = F,
    aclaraciones = "hay una diferencia de decimales en el dato agregado en subsector -Emisiones directas e indirectas de N2O y otros-. Está mal la base del analista",
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("sector" = "sector","subsector" = "subsector"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )

