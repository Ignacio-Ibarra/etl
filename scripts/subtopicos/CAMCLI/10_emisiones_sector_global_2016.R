################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_sector_global"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

f1 <- "R471C307"
df <- read_fuente_clean(f1)

df <- df %>% 
  filter(anio == max(anio))

df <- df %>% 
  mutate(valor_en_porcent = round(100*value/sum(value), 5)) %>% 
  select(-value)

df <- df %>% 
  rename(sector = sector_lv1, subsector = sector_lv2 , subsubsector = sector_lv3 )

df <- df %>% 
  group_by(sector) %>% 
  mutate(subsector = 
           case_when(
             n_distinct(subsector) == 1 ~ subsubsector,
             T ~ subsector
           )) %>% 
  ungroup()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


df_anterior <- descargar_output(nombre = "emisiones_sector_global_2016", 
                                subtopico = "CAMCLI")


comparacion <- argendataR::comparar_outputs(
  df = df,
  df_anterior = df_anterior,
  pk = c("sector","subsector","subsubsector"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df %>%
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list('nombre_nuevo' = output_name,
                                'nombre_anterior' = 'emisiones_sector_global_2016'),
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = f1,
    analista = "",
    pk = c("sector","subsector","subsubsector"),
    es_serie_tiempo = F,
    #columna_indice_tiempo = "",
    #columna_geo_referencia = "",
    nivel_agregacion = "global",
    aclaraciones = "Cambio de fuente. Se usa https://www.unep.org/resources/emissions-gap-report-2025 ",
    etiquetas_indicadores = list(sector = "Sector actividad nivel 1",
                                 subsector = "Sector actividad nivel 2",
                                 subsubsector = "Sector actividad nivel 3",
                                 anio = "Año",
                                 "valor_en_porcent"="valor en porcentaje de las emisiones GEI netas por sector"),
    unidades = list("valor_en_porcent" = "porcentaje")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")


