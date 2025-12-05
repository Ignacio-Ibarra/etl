################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

old_name <- "emisiones_piup_1990_2018"
output_name <- "emisiones_piup_arg"
#-- Librerias ----

#-- Lectura de Datos ----

fuente <- "R131C55" 

# traigo la data 
emisiones_arg <- read_fuente_clean(55)

emisiones_arg <- emisiones_arg %>% 
  mutate(anio = as.numeric(anio))


#-- Parametros Generales 
#-- Procesamiento ----

emisiones_arg <- emisiones_arg %>%
  filter(sector == "Procesos industriales y uso de productos") 

# equivalencias vs 2018
# Categoria	= Actividad
# Subcategoria 1er Orden = Subactividad
# Fuente = Categoria

emisiones_arg <- emisiones_arg %>% 
  mutate(sector = "Procesos industriales y uso de productos") %>% 
  mutate(subsector = case_when(
    actividad =="Industria de los minerales" ~ "Industria de los minerales",
    actividad =="Industria química" ~ "Industria química",
    actividad =="Industria de los metales" ~ "Industria de los metales",
    str_detect(actividad, "Uso de productos") ~ "Otros",
    TRUE ~ NA_character_)) %>% 
  group_by(anio, sector, subsector) %>% 
  summarise(valor_en_mtco2e = round(sum(valor_en_mtco2e, na.rm = TRUE), 2)) %>% 
  drop_na() 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_output <- emisiones_arg

df_anterior <- descargar_output(nombre=old_name,
                                subtopico = "CAMCLI",
                                entrega_subtopico = "datasets_segunda_entrega")

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
    fuentes = c(fuente),
    control = comparacion,
    analista = "",
    pk = c("anio","sector","subsector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("sector" = "sector de origen de emisiones",
                                 "subsector" = "subsector de origen de emisiones",
                                 "anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")



