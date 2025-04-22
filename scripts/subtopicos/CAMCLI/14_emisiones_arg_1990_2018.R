################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_arg_1990_2018"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
## descargo fuente raw para aergentina 

descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_1990_2018_arg <- readxl::read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) %>% 
  janitor::clean_names()
  
#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

emis_1990_2018_arg_final <- emis_1990_2018_arg %>%
  group_by(ano) %>%
  summarise(valor_en_mtco2e = round(sum(valor, na.rm = TRUE), 2)) %>% 
  rename(anio=ano)



df_anterior <- descargar_output(nombre=output_name,
                               subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- emis_1990_2018_arg_final

comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R131C0"),
    analista = "",
    pk = c("anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("anio" = "Año", "valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente"),
    aclaraciones = "Sin cambios"
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")

