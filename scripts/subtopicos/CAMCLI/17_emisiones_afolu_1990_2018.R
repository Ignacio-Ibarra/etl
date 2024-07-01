################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

output_name <- "emisiones_afolu_1990_2018"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_1990_2018_arg_afolu<- readxl::read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) %>% 
  janitor::clean_names()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

emis_1990_2018_arg_afolu_final <- emis_1990_2018_arg_afolu %>%
  filter(sector == "Agricultura, ganadería, silvicultura y otros usos de la tierra") %>%
  mutate(sector = "AGSyOUT") %>% 
  mutate(subsector = case_when(
    subcategoria_1er_orden %in% c("Emisiones directas de N2O de los suelos gestionados", 
                                  "Emisiones indirectas de N2O de los suelos gestionados", 
                                  "Emisiones indirectas de N2O resultantes de la gestión del estiércol", 
                                  "Cultivo de Arroz") ~ "Emisiones directas e indirectas de N2O y otros",
    subcategoria_1er_orden == "Emisiones de la quema de biomasa" ~ "Emisiones de la quema de biomasa",
    categoria == "Ganado" ~ "Ganado",
    categoria == "Tierra" ~ "Tierras",
    TRUE ~ NA_character_)) %>%
  group_by(ano, sector, subsector) %>%
  summarise(valor_en_mtco2e = round(sum(valor, na.rm = TRUE), 2)) %>% 
  rename(anio=ano) %>%
  drop_na() 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df <- emis_1990_2018_arg_afolu_final

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
                                            pk = c("anio"),
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
    control = comparacion,
    pk = c("anio","sector","subsector"),
    es_serie_tiempo = F,
    columna_indice_tiempo = "anio",
    aclaraciones = "Hay una diferencia contra el dataset del analista en la categoría Emisiones directas e indirectas de N2O y otros de la variable subsector. En el script de explicación de como se arma el dataset explicita que suma a la categría antes mencionada la fuente de emisón cultivos de arroz, pero en algunos años no la está sumando. El total para la categoría primeramente mencioanda es de 1095.67 para analista y de 1022,55 en este dataset producto del scripting",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )

