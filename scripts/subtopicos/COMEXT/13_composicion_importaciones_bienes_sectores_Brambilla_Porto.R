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

df_output <- comex_sectores_brambilla_porto %>% 
  dplyr::select(year, iso3, country_name_abbreviation, sector_bp, sector_bp_name, import_value_pc)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega")


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("year", "iso3", "sector_bp", "sector_bp_name"))

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  rename(anio = year) %>% # Modifico colname -year
  argendataR::write_output(
    control = comparacion, 
    directorio = "data/COMEXT/", 
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R113C57"),
    analista = analista,
    pk = c("anio", "iso3", "sector_bp", "sector_bp_name"),
    es_serie_tiempo = FALSE,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("import_value_pc" = "Importaciones de bienes (% del total importado en bienes)"),
    unidades = list("import_value_pc" = "porcentaje"), 
    aclaraciones = "los valores para ARG que son los graficados son
    bastante similares. La comparacion muestra outliers altos de algunas unidades geograficas no relevantes para la narrativa y valores malos de los tests."
  )

