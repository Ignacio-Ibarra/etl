################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

code_name <- str_split_1(rstudioapi::getSourceEditorContext()$path, pattern = "/") %>% tail(., 1)


output_name <- stringr::str_sub(string = code_name, start = 3, end = -3)

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
## Consolidado WDI para COMEXT
wdi_comext <- readr::read_csv(argendataR::get_temp_path("R98C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

df_output <- wdi_comext %>% 
  select(time = year, iso3, countryname, exportsconstant_goods_v2, exportsconstant_servi_v2) 


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso




df_anterior <- descargar_output(nombre = output_name, subtopico = "COMEXT", entrega_subtopico = "datasets_primera_entrega") 


comparacion <- argendataR::comparar_outputs(df = df_output, df_anterior = df_anterior,
                                            pk = c("time", "iso3", "countryname"))







comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("time", "iso3"),
  drop_output_drive = F
)

#-- Exportar Output ----


df_output <- df_output %>% 
  rename(anio = time)


# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    directorio = 'data/COMEXT/',
    control = comparacion, 
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c("R98C0"),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais/region",
    etiquetas_indicadores = list("exportsconstant_goods_v2" = "Exportaciones de bienes (millones de USD constantes 2015)"),
    unidades = list("exportsconstant_servi_v2" = "Exportaciones de servicios (millones de USD constantes 2015)")
  )

