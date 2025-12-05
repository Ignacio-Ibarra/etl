################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

output_name <- "emisiones_afolu_arg"
old_name <- "emisiones_afolu_1990_2018"

#-- Librerias ----

#-- Lectura de Datos ----

  #-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

fuente <- "R131C55" 

# traigo la data 
emisiones_arg <- read_fuente_clean(55)

emisiones_arg <- emisiones_arg %>% 
  mutate(anio = as.numeric(anio))


#-- Parametros Generales ----

emisiones_arg <- emisiones_arg %>%
  filter(sector %in% c("Agricultura y ganadería", "Usos de la tierra, cambios de uso de la tierra y silvicultura")) 

# equivalencias vs 2018
# Categoria	= Actividad
# Subcategoria 1er Orden = Subactividad
# Fuente = Categoria

emisiones_arg <- emisiones_arg %>%
  mutate(sector = "AGSyOUT") %>% 
  mutate(subsector = case_when(
    str_detect(categoria, "Quema") ~ "Emisiones de la quema de biomasa",
    actividad == "Ganadería" ~ "Ganado",
    actividad == "Tierra" ~ "Tierras",
    T ~ "Emisiones directas e indirectas de N2O y otros")) %>%
  group_by(anio, sector, subsector) %>%
  summarise(valor_en_mtco2e = round(sum(valor_en_mtco2e, na.rm = TRUE), 2)) %>% 
  ungroup()

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- emisiones_arg

df_anterior <- descargar_output(nombre=old_name,
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
    cambio_nombre_output = list(nombre_nuevo = output_name,
                                nombre_anterior = old_name),
    subtopico = "CAMCLI",
    control = comparacion,
    fuentes = c(fuente),
    analista = "",
    pk = c("anio","sector","subsector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    aclaraciones = "Actualizacion de fuente",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("sector" = "sector de origen de emisiones",
                                 "subsector" = "subsector de origen de emisiones",
                                 "anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")



