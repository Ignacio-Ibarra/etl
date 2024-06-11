################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

#-- Descripcion ----
#' Breve descripcion de output creado

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)


#-- Lectura de Datos ----



# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

comex_sectores_brambilla_porto <- readr::read_csv(argendataR::get_temp_path("R113C57"))



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- comex_export_sectores_brambilla_porto %>% 
  dplyr::select(year, iso3, country_name_abbreviation, sector_bp, sector_bp_name, export_value_pc)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(subtopico = "COMEXT",
  df_output,
  nombre = output_name,
  pk = c("year", "iso3", "sector_bp", "sector_bp_name")
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  rename(anio = year) %>% # Modifico colname -year
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R113C57"),
    analista = analista,
    pk = c("anio", "iso3", "sector_bp", "sector_bp_name"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("export_value_pc" = "Exportaciones de bienes (% del total exportado en bienes)"),
    unidades = list("export_value_pc" = "porcentaje")
  )

