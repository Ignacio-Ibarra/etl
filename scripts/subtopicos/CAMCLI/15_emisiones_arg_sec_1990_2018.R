################################################################################
##                              Dataset: nombre                               ##
################################################################################

#-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_arg_sec"
old_name = "emisiones_arg_sec_1990_2018"


fuente <- "R131C55" 

# traigo la data 
emisiones_arg <- read_fuente_clean(55)

emisiones_arg <- emisiones_arg %>% 
  mutate(anio = as.numeric(anio))

emisiones_arg <- emisiones_arg %>% 
  mutate(geonombreFundar = "Argentina", geocodigoFundar = "ARG") %>% 
  mutate(sector = case_when(
    sector == "Agricultura y ganadería" ~ "AGSyOUT",
    str_detect(sector, "sos de la tierra") ~ "AGSyOUT",
    T ~ sector
  ))

emisiones_arg <- emisiones_arg %>% 
  summarise(valor_en_mtco2e = sum(valor_en_mtco2e), .by = c(anio, geonombreFundar, geocodigoFundar, sector))



df_anterior <- descargar_output(nombre=old_name,
                                subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(emisiones_arg,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio", "sector"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

emisiones_arg %>%
  argendataR::write_output(
    output_name = output_name,
    cambio_nombre_output = list(nombre_nuevo = output_name,
                             nombre_anterior = old_name),
    control = comparacion,
    subtopico = "CAMCLI",
    fuentes = c(fuente),
    analista = "",
    pk = c("anio","sector"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("geonombreFundar" = "nombre geografico",
                                 "geocodigoFundar" = "codigo geonomenclador",
                                 sector = "sector de origen de emisiones",
                                 "anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )



mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")
