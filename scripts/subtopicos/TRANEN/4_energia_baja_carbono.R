################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
limpiar_temps()
rm(list = ls())

output_name <- "energia_baja_carbono.csv"
subtopico <- "TRANEN"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_raw_path("R71C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----


data <- data %>% 
  select(anio, iso3 = entities_code, valor_en_porcentaje = valor) %>% 
  filter(!is.na(iso3))

df_output <- data

df_output$iso3 %>% check_iso3()

df_output <- df_output %>% 
  mutate(iso3 = case_when(
    iso3 == "OWID_WRL" ~ "WLD",
    T ~ iso3
  )) %>% 
  rename(geocodigoFundar = iso3)

geonomen <- get_nomenclador_geografico_front() 

geonomen <- geonomen %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- left_join(df_output, geonomen)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(output_name, subtopico )


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  pk = c("anio", "geocodigoFundar"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 

descripcion <- argendataR::armador_descripcion(metadatos,
                                               output_cols = c("valor_en_porcentaje"))                     


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R71C0"),
    analista = "",
    control = comparacion,
    pk = c("anio", "geocodigoFundar"),
    aclaraciones = "Proporción del consumo de energía primaria que proviene de fuentes bajas en carbono",
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    descripcion_columnas = c(descripcion, geocodigoFundar = "Codigo geo", geonombreFundar = "Nombre geografico", anio = "año"),
    etiquetas_indicadores = list("valor_en_porcentaje" = "Porcentaje del consumo primario de energía"),
    unidades = list("valor_en_porcentaje" = "porcentaje")
  )


mandar_data(paste0(gsub("\\.csv$", "", output_name), ".csv"), subtopico = "TRANEN", branch = "main")
mandar_data(paste0(gsub("\\.csv$", "", output_name), ".json"), subtopico = "TRANEN",  branch = "main")




rm(list = ls())