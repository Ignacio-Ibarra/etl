################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "INFDES"
output_name <- "tasa_informalidad_productive_legal_ultimo_anio_latam"
fuente1 <- "R115C31"
fuente2 <- "R115C32"


#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
inf_prod_def <- readr::read_csv(argendataR::get_temp_path(fuente1))
inf_legal_def <- readr::read_csv(argendataR::get_temp_path(fuente2))

#-- Procesamiento ----

informality_df <- bind_rows(inf_prod_def, inf_legal_def) %>% dplyr::filter((apertura == "Edad total") & (serie == "Serie original"))

informality_df <- informality_df %>% 
  mutate(tematica = ifelse(tematica == "Informalidad", "Informalidad (definición legal)", tematica),
         variable = str_replace(variable,"Proporción", "Participación"))

informality_df <- informality_df %>% dplyr::filter(anio>2015)

df_output <- informality_df %>% 
  group_by(pais, tematica) %>% 
  mutate(dummy_anio_max = max(anio) == anio) %>% 
  ungroup() %>% 
  filter(dummy_anio_max) %>% 
  select(iso3, pais, anio, tipo_informalidad = tematica, valor)


 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("iso3", "anio","tipo_informalidad"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    pk = c("iso3", "anio","tipo_informalidad"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Tasa de informalidad"),
    unidades = list("valor" = "porcentaje")
  )

