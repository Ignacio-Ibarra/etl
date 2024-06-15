################################################################################
##                              Dataset: nombre                               ##
################################################################################

   #-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_arg_mundo"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

emis_arg_mundo_2016 <- readr::read_csv (argendataR::get_temp_path("R125C62"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----
#-- Controlar Output ----

df <- emis_arg_mundo_2016

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- df
  
comparacion <- argendataR::comparar_outputs(df,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("region"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = "emisiones_arg_mundo",
    subtopico = "CAMCLI",
    fuentes = c("R125C62"),
    analista = "",
    pk = c("region","sector"),
    es_serie_tiempo = F,
    columna_indice_tiempo = "",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("region"="Region","sector" = "Sector", "valor_en_porcent"="Valor de cada sector como porcentaje sobre total"),
    unidades = list("valor_en_porcent" = "Porcentaje de emisiones de CO2")
  )




