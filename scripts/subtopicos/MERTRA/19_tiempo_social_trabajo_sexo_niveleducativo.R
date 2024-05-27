################################################################################
##        Dataset: Tiempo social diario (en minutos) dedicado al trabajo no   ##
##        remunerado por sexo y nivel educativo, 14 años y más, 2021          ##
################################################################################

limpiar_temps()

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#-- Descripcion ----
#' Breve descripcion de output creado
#'

subtopico <- "MERTRA"
output_name <- "tiempo_social_trabajo_sexo_niveleducativo"
fuente1 <- "R93C19"
fuente2 <- "R93C20"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
enut_df_c5 <- readr::read_csv(argendataR::get_temp_path(fuente1))
enut_df_c6 <- readr::read_csv(argendataR::get_temp_path(fuente2))

#-- Procesamiento ----

df_output <- enut_df_c5 %>% 
  left_join(enut_df_c6, by=join_by(sexo, nivel_ed, tipo_trabajo)) %>% 
  mutate(minutos = participacion * minutos_dia) %>%
  dplyr::filter(tipo_trabajo == "No remunerado") %>% 
  select(sexo, nivel_educativo = nivel_ed, minutos) 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("sexo", "nivel_educativo"),
  drop_output_drive = F
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
    pk =  c("sexo", "nivel_educativo"),
    es_serie_tiempo = F,
    etiquetas_indicadores = list("minutos" = "Promedio de minutos diarios dedicados al trabajo"),
    unidades = list("minutos" = "unidades")
  )

