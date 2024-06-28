################################################################################
##                  Dataset: Tasa de Actividad por año y país                 ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "tasa_actividad_por_pais_anio"
fuente1 <- "R45C0"
fuente2 <- "R46C0"


#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
df_lab_force <- readr::read_csv(argendataR::get_temp_path(fuente1))
df_pop <- readr::read_csv(argendataR::get_temp_path(fuente2)) 
geonomenclador <- argendataR::get_nomenclador_geografico()


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_lab_force <- df_lab_force %>% 
  select(iso3 = iso3c, anio = year,  lab_force = SL.TLF.TOTL.IN) %>% 
  dplyr::filter(!is.na(iso3))

df_pop <- df_pop %>% 
  select(iso3 = iso3c, anio = year,  total_pop = SP.POP.TOTL) %>% 
  dplyr::filter(!is.na(iso3))

df <- inner_join(df_lab_force, df_pop) 

df <- df %>% 
  mutate(tasa_actividad = lab_force / total_pop)

geonomenclador <- geonomenclador %>% 
  select(codigo_fundar, iso3_desc = desc_fundar, nivel_agregacion)

df_output <- df %>% 
  select(iso3, anio, tasa_actividad) %>% 
  left_join(., geonomenclador, by=c("iso3"="codigo_fundar")) %>% 
  dplyr::filter(!is.na(nivel_agregacion))



#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso



df_anterior <- descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            nombre = output_name,  pk = c("anio", "iso3"))



#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion,
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "países y regiones de países",
    etiquetas_indicadores = list("tasa_actividad" = "Ratio entre la cantidad de personas pertenecientes a la población económicamente activa y la población total"),
    unidades = list("tasa_actividad" = "porcentaje")
  )

