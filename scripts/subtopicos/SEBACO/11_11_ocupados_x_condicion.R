################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "11_ocupados_x_condicion"
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
  filter(ciiu_rev3_4d %in% c('2213',
                             '3530',
                             '7210','7220','7230','7240','7290',
                             '7300',
                             '7410','7421','7430','7494',
                             '7491',
                             '9211'
                             #,'7499' # Se agregan "Servicios empresariales ncp, pese a que pueden tomar actividades que no estan dentro de SBC
  ))

empleo_sbc <- empleo_sbc %>% 
  mutate(rama_de_actividad = case_when(ciiu_rev3_4d %in% c('7210','7220','7230','7240','7290') ~ 'SSI',
                                       ciiu_rev3_4d %in% c('7300') ~ 'Investigación y desarrollo',
                                       ciiu_rev3_4d %in% c('7410') ~ 'Ss. Jurídicos y de contabilidad',
                                       ciiu_rev3_4d %in% c('7421') ~ 'Ss. Arquitectura',
                                       ciiu_rev3_4d %in% c('7430') ~ 'Ss. publicidad',
                                       TRUE ~ 'Otras'))
empleo_sbc$ciiu_rev3_4d <- NULL

# Paso 3: Calcular la suma de puestos privados solo para SBC por año
sbc_puestos <- empleo_sbc %>%
  group_by(anio,rama_de_actividad) %>%
  summarize(puestos_sbc = sum(cant_promedio_puestos_privados, na.rm = TRUE))

sbc_puestos <- sbc_puestos %>% 
  group_by(anio) %>% 
  mutate(sbc = sum(puestos_sbc)) %>% 
  ungroup() %>% 
  mutate(prop_sbc = puestos_sbc / sbc)
sbc_puestos$sbc <- NULL
sbc_puestos$empleo <- NULL
sbc_puestos <- sbc_puestos %>% 
  rename(prop_rama = prop_sbc)
sbc_puestos <- sbc_puestos %>% 
  rename(rama = rama_de_actividad) %>% 
  select(rama,anio,prop_rama)


# Limpiar nombres de columnas 
df_clean <- sbc_puestos 

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
  pk = c("anio","rama"), # variables pk del dataset para hacer el join entre bases
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
    pk = c("anio","rama"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("rama"="Rama de actividad",
                                 "anio"="Año de referencia",
                                 "prop_rama"="Composición del empleo en SBC (proporción explicada por cada rama"),
    unidades = list("prop_rama" = "proporción")
  )


#temp_files <- list.files(temp_dir, full.names = TRUE)
#view(temp_files)
#list.files(tempdir())

