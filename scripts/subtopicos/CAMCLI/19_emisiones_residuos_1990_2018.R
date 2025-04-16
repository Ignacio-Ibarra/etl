#################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

output_name <- "emisiones_residuos_1990_2018"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_1990_2018_arg_residuos<- readxl::read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) %>% 
  janitor::clean_names()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

emis_1990_2018_arg_residuos_final <- emis_1990_2018_arg_residuos %>%
  filter(sector == "Residuos") %>%
  mutate(sector = "Residuos") %>% 
  mutate(subsector = case_when(
    categoria %in% c("Eliminación de residuos sólidos",
                     "Incineración de residuos",
                     "Tratamiento biológico de los Residuos sólidos") ~ "Residuos sólidos",
    categoria=="Tratamiento y eliminación de aguas residuales" ~ "Aguas residuales",
    TRUE ~ NA_character_)) %>%
  group_by(ano, sector, subsector) %>%
  summarise(valor_en_mtco2e = round(sum(valor, na.rm = TRUE), 2)) %>% 
  rename(anio=ano) %>%
  drop_na() 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- emis_1990_2018_arg_residuos_final

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio", "sector", "subsector"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    control = comparacion,
    fuentes = c("R131C0"),
    analista = "",
    pk = c("anio","sector","subsector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "dev")





