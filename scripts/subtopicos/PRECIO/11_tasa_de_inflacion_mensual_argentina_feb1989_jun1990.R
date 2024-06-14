################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "11_tasa_de_inflacion_mensual_argentina_feb1989_jun1990.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

ipc_gba <- readr::read_csv(argendataR::get_temp_path("R128C53"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


ipc_gba <- ipc_gba %>%
  select(anio, mes , nivel_general) %>% 
  mutate(anio = as.numeric(anio),
         fecha = lubridate::ym(paste(anio, mes, sep = "-"))) %>% 
  mutate(var_ipc_trim = (nivel_general/lag(nivel_general, 1)-1)*100) %>% 
  filter( fecha >= lubridate::ym("1989-2") & fecha <= lubridate::ym("1990-6")) %>% 
  select(-c(mes, anio, nivel_general)) 

df_output <- ipc_gba %>%
  rename(inflacion_mensual = var_ipc_trim)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <-  descargar_output(nombre = output_name,
                   subtopico = "PRECIO",
                   entrega_subtopico = "datasets_update")

meses.nombres <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio","Julio",
                   "Agosto","Septiembre","Octubre","Noviembre","Diciembre")

meses.abbr <- tolower(substr(meses.nombres, 1, 3))

df_anterior$mes <- sapply(df_anterior$trimestre, 
                          function(x) which(substr(x, 1, 3) == meses.abbr))

df_anterior$fecha <- paste0("19", substr(df_anterior$trimestre, 5, 6), "-", df_anterior$mes)

df_anterior <- df_anterior %>% 
  mutate(fecha = lubridate::ym(fecha)) %>% 
  select(fecha, inflacion_mensual)

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c("fecha"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R128C53"),
    analista = "",
    pk = c("fecha"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("inflacion_mensual" = "Inflación mensual"),
    unidades = list("inflacion_mensual" = "porcentaje")
  )

