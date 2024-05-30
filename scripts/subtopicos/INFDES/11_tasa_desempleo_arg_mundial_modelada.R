################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_desempleo_arg_mundial_modelada"
fuente1 <- "R109C0" # SL.UEM.TOTL.ZS

#-- Librerias ----


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
desempleo_total.df <- readr::read_csv(argendataR::get_temp_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year,  tasa_desempleo = SL.UEM.TOTL.ZS) %>% 
  mutate(tasa_desempleo = tasa_desempleo/100) %>% 
  filter(anio>1990) %>% 
  filter(!is.na(iso3))

geonomenclador <- argendataR::get_nomenclador_geografico()

df_output <- desempleo_total.df %>% 
  filter(iso3  %in% c('ARG','WLD')) %>% 
  left_join(geonomenclador %>% select(iso3 = codigo_fundar, pais_desc = desc_fundar), by = join_by(iso3)) 




#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  subtopico = subtopico,
  nombre = output_name,
  pk = c("anio", "iso3"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = "",
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("tasa_desempleo" = "Desempleo como proporción de la fuerza laboral"),
    unidades = list("tasa_desempleo" = "unidades")
  )

