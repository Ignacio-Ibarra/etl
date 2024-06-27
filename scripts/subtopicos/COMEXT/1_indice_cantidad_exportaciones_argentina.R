################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 3, end = -3)

#-- Librerias ----
library(janitor) # Simple Tools for Examining and Cleaning Dirty Data CRAN v2.2.0 

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_COMEXT.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:


ferreres <- readr::read_csv(argendataR::get_temp_path("R43C22")) %>%
  dplyr::rename(cantidades_exportacion_ferreres = cantidades_de_exportacion) 

indec <-read_csv(argendataR::get_temp_path("R44C23")) %>%
  dplyr::rename(cantidades_exportacion_indec = cantidades_de_exportacion) 


#-- Procesamiento ----

df_output <-  ferreres %>% 
  dplyr::full_join(indec, by = 'anio')


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  subtopico = 'COMEXT', df = df_output,
  nombre = output_name,
  pk = c("anio"),
  drop_output_drive = F
)



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("anio"))






#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    directorio = 'data/COMEXT/',
    control = comparacion, 
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R43C22", "R44C23"),
    analista = analista,
    pk = c("anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("cantidades_exportacion_ferreres" = "Cantidades de Exportacion ARG (Serie Ferreres - Fundacion Norte y Sur)",
                                 "cantidades_exportacion_indec" = "Cantidades de Exportacion ARG (Serie INDEC)"),
    unidades = list("cantidades_exportacion_ferreres" = "Indice (base 2004)",
                    "cantidades_exportacion_indec" = "Indice (base 2004)")
  )



