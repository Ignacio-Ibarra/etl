################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_energia_1990_2018"

#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

descargar_fuente_raw(id_fuente = 131, tempdir())

# traigo la data 
emis_1990_2018_arg_energia_subsector<- readxl::read_xlsx (argendataR::get_temp_path("R131C0"),skip = 1) %>% 
  janitor::clean_names()

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

emis_1990_2018_arg_energia_subsector_final <- emis_1990_2018_arg_energia_subsector %>%
  filter(sector=="Energía") %>% 
  mutate(subsector = case_when(
    subcategoria_1er_orden == "Industrias de la energía" ~ "Industrias de la energía",
    subcategoria_1er_orden == "Industrias manufactureras y de la construcción" ~ "Industrias manufactureras y de la construcción",
    subcategoria_1er_orden == "Transporte" ~ "Transporte",
    subcategoria_1er_orden == "Otros sectores" ~ "Otros sectores",
    subcategoria_1er_orden %in% c("Combustibles sólidos", "Petróleo y gas natural") ~ "Emisiones fugitivas provenientes de la fabricación de combustibles",
    TRUE ~ NA_character_  # En caso de que no se cumpla ninguna de las condiciones anteriores
  )) %>% 
  group_by(ano, sector, subsector) %>%
  summarise(valor_en_mtco2e = round(sum(valor, na.rm = TRUE), 2)) %>% 
  rename(anio=ano)

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df <- emis_1990_2018_arg_energia_subsector_final

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
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c("R131C0"),
    analista = "",
    control = comparacion,
    pk = c("anio","sector","subsector"),
    es_serie_tiempo = F,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )
