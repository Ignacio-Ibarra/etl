################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "ponderadores_engho_evolucion.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
df <- readr::read_csv(argendataR::get_temp_path("R134C61"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df <- df %>% 
  filter(subcategoria == "Total" & grepl("Porcentaje", indicador) & indicador != "Total gasto de consumo")

df <- df %>% 
  select(rubro = categoria, periodo, porcentaje = valor)

df <-  df %>% 
  mutate(porcentaje = porcentaje/100)

df_output <- df

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "PRECIO", entrega_subtopico = "datasets_update",
  pk = c("rubro", "periodo"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R134C61"),
    analista = "",
    pk = c("rubro", "periodo"),
    es_serie_tiempo = F,
    # columna_indice_tiempo = "anio",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("porcentaje" = "Porcentaje que el rubro representa en el gasto de consumo de los hogaores"),
    unidades = list("porcentaje" = "porcentaje")
  )

