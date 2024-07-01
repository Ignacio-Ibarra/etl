################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "01_evolucion_CO2_historico"

#-- Librerias ----

#-- Lectura de Datos ----
# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# traigo data de años ára atrás al presente 137 al 800.000 aprox y le sumo data de 1958 a 2024

co2_hist<-read_csv(argendataR::get_temp_path("R100C26"))
co2_1958_2024<-read_csv(argendataR::get_temp_path("R161C71"))

#-- Procesamiento ----

# Creo nuevo dataframe con las columnas "fecha_estimada" y "co2_ppm" del segundo dataset
co2_1958_2024_nuevo <- co2_1958_2024 %>%
  select(anio, CO2_ppmv_deseasonalized) %>%
  rename(fecha_estimada = anio, co2_ppm = CO2_ppmv_deseasonalized)

# junto con co2_hist

co2_hist_y_1958_2024 <- co2_hist %>%
  bind_rows(co2_1958_2024_nuevo)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df <- co2_hist_y_1958_2024

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- df

comparacion <- argendataR::comparar_outputs(df,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("fecha_estimada"),
                                            drop_joined_df = F)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R100C26","R161C71"),
    analista = "",
    control = comparacion,
    pk = c("fecha_estimada"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "fecha_estimada",
    aclaraciones = "luego de darles vueltas al tema este gráfico se dibuja por diseño. pero de todas formas a co2_hist que va de -137 a -900 mil le trajimos la data de 1958 a 2024",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "global",
    etiquetas_indicadores = list("fecha_estimada" = "Fecha estimada","co2_ppm"="Evolución histórica de CO2"),
    unidades = list("co2_ppm" = "Partes por millón")
  )

