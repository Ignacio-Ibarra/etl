################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "MERTRA"
output_name <- "tasa_participacion_censos"
fuente1 <- "R108C27"


#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
df_output <- readr::read_csv(argendataR::get_temp_path(fuente1))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "datasets_primera_entrega")

comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
  nombre = output_name,
  pk = c("anio", "sexo")
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    control = comparacion,
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = "",
    pk = c("anio", "sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    nivel_agregacion = "nacional",
    etiquetas_indicadores = list("tasa_participacion" = "Tasa de participación laboral"),
    unidades = list("tasa_participacion" = "unidades")
  )

