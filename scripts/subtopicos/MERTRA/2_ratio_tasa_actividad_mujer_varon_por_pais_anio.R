################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


output_name <- "ratio_tasa_actividad_mujer_varon_por_pais_anio"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
df <- readr::read_csv(argendataR::get_temp_path("R42C0")) 
geonomenclador <- argendataR::get_nomenclador_geografico()


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df <- df %>% select(iso3 = iso3c, iso3_desc = country, anio = year,  ratio_tasa_actividad_mujer_varon = SL.TLF.CACT.FM.NE.ZS)
geonomenclador <- geonomenclador %>% select(codigo_fundar, nivel_agregacion)
df_output <- df %>% left_join(., geonomenclador, by=c("iso3"="codigo_fundar")) %>% filter(!is.na(nivel_agregacion))


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  subtopico = "MERTRA",
  nombre = output_name,
  pk = c("anio", "iso3"),
  drop_output_drive = F
)



#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "MERTRA",
    fuentes = c("R42C0"),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "países y regiones de países",
    etiquetas_indicadores = list("ratio_tasa_actividad_mujer_varon" = "Ratio entre la tasa de actividad femenina y la tasa de actividad masculina"),
    unidades = list("ratio_tasa_actividad_mujer_varon" = "porcentaje")
  )

