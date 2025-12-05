################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

old_name <- "emisiones_arg_1990_2018"
output_name <- "emisiones_arg"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
## descargo fuente raw para aergentina 

f2 <- "R131C55"

# traigo la data 
emisiones_arg <- read_fuente_clean(55)



emisiones_arg <- emisiones_arg %>%
  mutate(anio = as.numeric(anio)) %>% 
  group_by(anio) %>%
  summarise(valor_en_mtco2e = round(sum(valor_en_mtco2e, na.rm = TRUE), 2)) 


emisiones_arg <- emisiones_arg %>%
  mutate(geonombreFundar = "Argentina",
         geocodigoFundar = "ARG")

df_anterior <- descargar_output(nombre=old_name,
                               subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- emisiones_arg

comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list(
      'nombre_nuevo' = output_name,
      'nombre_anterior' = old_name
    ),
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c(f2),
    analista = "",
    pk = c("anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("geocodigoFundar" = "geocodigoFundar",
                                 "geonombreFundar" = "nombre geografico",
                                 "anio" = "Año", "valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente"),
    aclaraciones = "Acutalizado"
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")

