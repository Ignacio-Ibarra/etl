################################################################################
##                              Dataset: nombre                               ##
################################################################################

limpiar_temps()

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#-- Descripcion ----
#' Breve descripcion de output creado
#'

subtopico <- "MERTRA"
output_name <- "trabajo_no_remunerado_sexo_internacional"
fuente1 <- "R96C24"



#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
jcharm_cleaned <- readr::read_csv(argendataR::get_temp_path(fuente1))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- jcharm_cleaned %>% 
  dplyr::filter(subtipo_actividad == "Trabajo no remunerado") %>% 
  group_by(iso3, pais_desc, anios_observados, continente_fundar) %>% 
  mutate(share_trabajo_no_remun = minutos / sum(minutos, na.rm = T)) %>% 
  dplyr::filter(sexo == "Mujeres") %>% 
  select(iso3, pais_desc, continente_fundar, anios_observados, share_trabajo_no_remun)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso



df_anterior <- descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            nombre = output_name,
                                            pk = c("iso3")
)



#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion,
    output_name = output_name,
    subtopico = subtopico,
    directorio = tempdir(),
    fuentes = c(fuente1),
    analista = "",
    pk = c("iso3"),
    es_serie_tiempo = F,
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("share_trabajo_no_remun" = "Proporción del tiempo social dedicado al trabajo no remunerado que es realizado por mujeres"),
    unidades = list("share_trabajo_no_remun" = "unidades")
  )

