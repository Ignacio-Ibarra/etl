################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "1_composicion_ipc_argentina.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
ponderaciones <- readr::read_csv(argendataR::get_temp_path("R117C29"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- ponderaciones %>% 
  filter(region == "Nacional") %>% 
  rename(sector = division, valor = peso_division) %>% 
  mutate(valor =  round(100*valor, 2),
         sector = replace_non_ascii(sector) %>% 
           gsub(",", "", .)) %>% 
  select(-c(region, peso_region))
  

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,subtopico = "PRECIO", entrega_subtopico = "datasets_update",
  pk = c("sector"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R117C29"),
    analista = "",
    control = comparacion,
    pk = c("sector"),
    es_serie_tiempo = F,
    # columna_indice_tiempo = "anio",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("valor" = "Peso de cada sector en el IPC a nivel nacional"),
    unidades = list("valor" = "porcentaje")
  )

