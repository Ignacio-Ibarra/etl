#################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

#-- Descripcion ----
#' Breve descripcion de output creado
#'
#'

old_name <- "emisiones_prov_2010_2018"
output_name <- "emisiones_prov_arg"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:

# descargar_fuente_raw(id_fuente = 157, tempdir())

# traigo la data 
fuente <- "R157C67"
emisiones_provincias <- read_fuente_clean(fuente)

emisiones_provincias %>% 
  distinct(sector, categoria) %>% view()

emisiones_provincias <- emisiones_provincias %>%
  mutate(anio =as.numeric(anio)) %>% 
  mutate(sector = gsub("Sector ", "", sector)) %>% 
  mutate(sector = case_when(
  sector == "Agricultura y ganadería" ~ "AGSyOUT",
  str_detect(sector, "Usos de la Tierra") ~ "AGSyOUT",
  str_detect(sector, "Procesos industriales y uso de productos") ~ "PIUP",
  T ~ sector
)) %>% 
  filter(!str_detect(sector, "Total") & str_detect(categoria, "Sector"))

emisiones_provincias <- emisiones_provincias %>%
  summarise(valor_en_mtco2e = sum(valor, na.rm = T),
            .by = c(provincia,  anio, sector) )

emisiones_provincias <- emisiones_provincias %>%
  mutate(provincia = case_when(
    provincia == "PBA" ~ "Buenos Aires",
    provincia == "Cordoba" ~ "Córdoba",
    provincia == "Entre Rios" ~ "Entre Ríos",
    provincia == "Neuquen" ~ "Neuquén",
    provincia == "Rio Negro" ~ "Río Negro",
    T ~ provincia
    
  ))



geo <- get_nomenclador_geografico_front()
geo <- geo %>% select(geocodigoFundar = geocodigo, geonombreFundar = name_long)

emisiones_provincias <- left_join(emisiones_provincias, geo, by = c("provincia" = "geonombreFundar"))

#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso

df_output <- emisiones_provincias %>% 
  mutate(valor_en_mtco2e = round(valor_en_mtco2e, 2)) %>% 
  rename(geonombreFundar = provincia)

df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(df_output,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("anio", "geonombreFundar", "sector"),
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
    pk = c("anio","sector","geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    #columna_geo_referencia = "iso3",
    nivel_agregacion = "sector",
    etiquetas_indicadores = list("sector" = "sector de origen de emisiones",
                                 geocodigoFundar = "codigo geonomenclador",
                                 geonombreFundar = "nombre geografico",
                                 "anio" = "Año","valor_en_mtco2e"="Emisiones de dioxido de carbono en toneladas"),
    unidades = list("valor_en_mtco2e" = "Millones de toneladas de CO2 equivalente")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")






