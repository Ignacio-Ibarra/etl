################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "10_tasa_trimestral_inflacion_y_pib_desestacionalizado_argentina_1984_1988.csv"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

pbi_sh <- readr::read_csv(argendataR::get_temp_path("R133C58"))

ipc_gba <- readr::read_csv(argendataR::get_temp_path("R128C53"))



#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

ipc_gba <- ipc_gba %>%
  select(anio, mes , nivel_general) %>% 
  mutate(anio = as.numeric(anio),
         fecha = lubridate::ym(paste(anio, mes, sep = "-"))) %>% 
  mutate(var_ipc_trim = (nivel_general/lag(nivel_general, 3)-1)*100) %>% 
  filter(mes %in% c(3,6,9,12) & year(fecha) %in% 1980:2005) %>% 
  mutate(trim = mes/3) %>% 
  select(-c(mes, fecha, nivel_general))

pbi_sh <- pbi_sh %>% 
  select(anio, trim, producto_interno_bruto_a_precios_de_mercado)


PIBest <- stats::ts(pbi_sh$producto_interno_bruto_a_precios_de_mercado, start=c(1980,1), frequency = 4)


PIBdesest <- seasonal::seas(PIBest, x11="")


pbi_sh$pib_pm_desest <- PIBdesest$data[,"seasonaladj"] %>% as.vector()

df <- ipc_gba %>% 
  left_join(pbi_sh)

df_output <- df %>% 
  filter(anio  %in% 1984:1988) %>% 
  mutate(trimestre = paste(trim, anio, sep = "q")) %>% 
  select(trimestre, inflacion_trimestral = var_ipc_trim, pbi_desestacionalizado_en_millones_de_pesos_de_1993 = pib_pm_desest)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_anterior <- descargar_output(nombre = output_name, subtopico = "PRECIO", entrega_subtopico = "datasets_update")

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior, 
  pk = c("trimestre"),
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "PRECIO",
    fuentes = c("R133C58", "R128C53"),
    analista = "",
    pk = c("trimestre"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "trimestre",
    # columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("inflacion_trimestral" = "Inflación trimestral",
                                 "pbi_desestacionalizado_en_millones_de_pesos_de_1993" = "PBI desestacionalizado"),
    unidades = list("inflacion_trimestral" = "porcentaje",
                    "pbi_desestacionalizado_en_millones_de_pesos_de_1993" = "milllones de pesos de 1993"
                    )
  )

