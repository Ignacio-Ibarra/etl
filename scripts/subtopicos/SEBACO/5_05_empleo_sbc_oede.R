################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "05_empleo_sbc_oede"
analista = "Nicolas Sidicaro"
fuente1 <- "R238C138"

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R"
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

#readr::read_csv(argendataR::get_temp_path("RXXCX"))

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

## traigo la data
data <- arrow::read_parquet(get_clean_path(fuente1))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

# Paso 1: Filtrar solo los valores donde 'rama_de_actividad' es 'Total'
totales_anuales <- data %>%
  filter(rama_de_actividad == 'Total' & !is.na(anio)) %>%
  select(anio, total_puestos = cant_promedio_puestos_privados)

# Paso 2: Filtrar las actividades específicas de SBC (Servicios Basados en Conocimientos)
empleo_sbc <- data %>%
  filter(ciiu_rev3_4d %in% c('2213', '3530', '7210', '7220', '7230', '7240', '7290',
                             '7300', '7410', '7421', '7430', '7491', '7494', '9211'))

# Paso 3: Calcular la suma de puestos privados solo para SBC por año
sbc_puestos <- empleo_sbc %>%
  group_by(anio) %>%
  summarize(puestos_sbc = sum(cant_promedio_puestos_privados, na.rm = TRUE))

# Paso 4: Unir las tablas de totales y SBC, y calcular la proporción de SBC
empleo_final <- sbc_puestos %>%
  left_join(totales_anuales, by = "anio") %>%
  mutate(prop_sbc = puestos_sbc / total_puestos)

# Limpiar nombres de columnas 
df_clean <- empleo_final %>%
  select(anio, sbc = puestos_sbc, prop_sbc)  

##########################

df_output <- df_clean 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

## data anterior

df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    control = comparacion,
    subtopico = subtopico,
    fuentes = c("R238C138"),
    analista = analista,
    pk = c("anio"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("sbc"="Empleados dentro de SBC","prop_sbc"="Proporción del empleo total explicado por SBC"),
    unidades = list("sbc" = "millones","prop_sbc"="porcentaje")
  )



#temp_files <- list.files(temp_dir, full.names = TRUE)
#view(temp_files)
#list.files(tempdir())

