################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
limpiar_temps()
rm(list = ls())

output_name <- "generacion_nuclear_twh.csv"
subtopico <- "TRANEN"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
data <- readr::read_csv(argendataR::get_raw_path("R73C0"))


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

data <- data %>% 
  select(anio, iso3 = entities_code, valor_en_twh = valor)

data <- data %>% 
  filter(!is.na(iso3))

df_output <- data


df_output$iso3 %>% check_iso3()

df_output <- df_output %>% 
  mutate(iso3 = case_when(
    iso3 == "OWID_WRL" ~ "WLD",
    iso3 == "OWID_KOS" ~ "XKX",
    T ~ iso3
  )) %>% 
  rename(geocodigoFundar = iso3)

geonomen <- get_nomenclador_geografico_front() 

geonomen <- geonomen %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

df_output <- left_join(df_output, geonomen)

check_iso3(df_output$geocodigoFundar)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  subtopico = "TRANEN",
  pk = c("anio", "geocodigoFundar"),
  drop_joined_df = F
)

#-- Exportar Output ----


# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 

# descripcion <- argendataR::armador_descripcion(metadatos,
#                                                output_cols = c("valor_en_twh"))                     



# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "TRANEN",
    fuentes = c("R73C0"),
    analista = "",
    control = comparacion,
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    descripcion_columnas =list("valor_en_twh" = "Generación nucleoeléctrica", 
                               "geocodigoFundar" = "Codigo geografico",
                               "geonombreFundar" = "Nombre geografico"),
    etiquetas_indicadores = list("valor_en_twh" = "Generación nucleoeléctrica"),
    unidades = list("valor_en_twh" = "TWh")
  )


mandar_data(paste0(gsub("\\.csv$", "", output_name), ".csv"), subtopico = "TRANEN", branch = "main")
mandar_data(paste0(gsub("\\.csv$", "", output_name), ".json"), subtopico = "TRANEN",  branch = "main")



rm(list = ls())