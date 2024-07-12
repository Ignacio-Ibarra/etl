################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
subtopico <- "COMEXT"

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 4, end = -3)

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

comex_baci_diferenciados_berinini <- readr::read_csv(argendataR::get_temp_path("R113C59"))



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- comex_baci_diferenciados_berinini %>% 
  # dplyr::filter(!is.na(microd)) %>% 
  dplyr::mutate(microd_name = dplyr::case_when(
    microd == "D" ~ 'Diferenciado', TRUE ~ 'No diferenciado'), 
                microd = dplyr::case_when(
                  microd == "D" ~ 1, TRUE ~ 2)) %>% 
  group_by(year, iso3, country_name_abbreviation = country_name, microd, microd_name) %>% 
  summarise(export_value_pc = sum(export_value_pc, na.rm = T))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 

df_anterior <- df_anterior %>% 
  dplyr::filter(year == 2020) # Reduzco comparacion solo para year 2020  (dataset original toda la serie 2007 / 2020)


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("year", "iso3", 'microd', 'microd_name'))


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion, 
    directorio = tempdir(),
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R113C57"),
    analista = "",
    pk = c("year", "iso3", 'country_name_abbreviation', 'microd', 'microd_name'),
    es_serie_tiempo = FALSE,
    columna_indice_tiempo = "year",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("export_value_pc" = "Exportaciones de bienes (% del total exportado en bienes)"),
    unidades = list("export_value_pc" = "porcentaje"), 
                           aclaraciones =  'Reduzco comparacion solo para year 2020  (dataset original analistas toda la serie 2007 / 2020)'
  )

