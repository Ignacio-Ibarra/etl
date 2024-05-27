################################################################################
##     Dataset: Media Movil de 5 años de la tasa de actividad, según la edad  ##
################################################################################

#-- Descripcion ----
#' Media Movil de 5 años de la tasa de actividad, según la edad
#'

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MERTRA"
output_name <- "media_movil_por_edad_de_tasa_actividad"
fuente1 <- "R49C16"   
# fuente2 <- "R84C14"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ephtu_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
# ephtu_df <- ephtu_df %>% rename_with(tolower, everything()) #esta linea no haría falta que esté cuando cambiemos el input de fuente1 por la fuente clean. 


#-- Procesamiento ----

ephtu_df <- ephtu_df %>% mutate(
  activo = case_when(
    estado == 1 | estado == 2 ~ 1,
    TRUE ~ 0
  ),
  # ocupado = case_when(
  #   estado == 1 ~ 1,
  #   TRUE ~ 0
  # )
)

# ephtu_df <- ephtu_df %>%
#   left_join(codigos, by = c('aglomerado', 'provincia'))


df_output <- ephtu_df %>% 
  select(anio = ano4, activo, edad = ch06, pondera) %>% 
  group_by(anio, edad, activo) %>% 
  summarize(pondera = sum(pondera)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = activo, values_from = pondera, values_fill = 0) %>% 
  rename(activo = `1`, no_activo = `0`) %>% 
  mutate(activo = zoo::rollsum(activo, 5, align="right", fill = 0),
         no_activo = zoo::rollsum(no_activo, 5, align = "right", fill = 0)) %>% 
  mutate(tasa_actividad = activo / (no_activo + activo)) %>% 
  dplyr::filter(edad >= 10 & edad <= 90) %>% 
  select(anio, edad, tasa_actividad)
  

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "edad"),
  drop_output_drive = F
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
    pk = c("anio", "edad"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    etiquetas_indicadores = list("tasa_actividad" = "Ratio entre la cantidad de personas pertenecientes a la población económicamente activa y la población total, por grupo etario (media movil cada 5 años de edad)."),
    unidades = list("tasa_actividad" = "porcentaje")
  )



