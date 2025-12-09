################################################################################
##                              Dataset: nombre                               ##
################################################################################

rm(list = ls())

   #-- Descripcion ----
#' Breve descripcion de output creado
#'

output_name <- "emisiones_arg_mundo"

#-- Librerias ----

#-- Lectura de Datos ----


## descargo fuente raw para mundo 
# descargar_fuente_raw(id_fuente = 125, tempdir())
f1 <- "R471C307"
f2 <- "R131C55"

# traigo la data 
emisiones_arg <- read_fuente_clean(55)

emisiones_arg <- emisiones_arg %>% 
  mutate(anio = as.numeric(anio)) %>% 
  filter(anio == max(anio)) 

emisiones_arg <- emisiones_arg %>% 
  mutate(geonombreFundar = "Argentina", geocodigoFundar = "ARG") %>% 
  mutate(sector = case_when(
    sector == "Agricultura y ganadería" ~ "AGSyOUT",
    str_detect(sector, "sos de la tierra") ~ "AGSyOUT",
    T ~ sector
  ))

emisiones_arg <- emisiones_arg %>% 
  summarise(valor = sum(valor_en_mtco2e), .by = c(anio, geonombreFundar, geocodigoFundar, sector)) %>% 
  mutate(valor_en_porcent = valor/sum(valor))


  

max_anio <- unique(emisiones_arg$anio)

# me quedo con el sheet 3 que es donde está la data de sectores 
emisiones_mundo <- read_fuente_clean(307)
  

emisiones_mundo <- emisiones_mundo %>% 
  mutate(anio = as.numeric(anio)) %>% 
    filter(
      anio == max_anio)
# limpio los nombres de las variables

emisiones_mundo <- emisiones_mundo %>% 
  mutate(geonombreFundar = "Mundo",
         geocodigoFundar = "WLD")


emisiones_mundo <- emisiones_mundo %>% 
  rename(sector = sector_lv1) %>% 
  summarise(valor = sum(value), .by = c(anio, geonombreFundar, geocodigoFundar, sector)) %>% 
  mutate(valor_en_porcent = valor/sum(valor))

emisiones_mundo <- emisiones_mundo %>% 
  mutate(sector = case_when(
                            sector =="Agricultura" ~ "AGSyOUT",
                            sector == "Procesos industriales" ~ "Procesos industriales y uso de productos",
                             T ~ sector
                               ))



df <- bind_rows(emisiones_arg, emisiones_mundo)

df <- df %>% 
  mutate(valor_en_porcent = valor_en_porcent * 100)


#-- Parametros Generales ----

# fechas de corte y otras variables que permitan parametrizar la actualizacion de outputs

#-- Procesamiento ----
#-- Controlar Output ----


df_anterior <- descargar_output(nombre=output_name,
                                subtopico = "CAMCLI")

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(df,
                                            df_anterior,
                                            k_control_num = 3,
                                            pk = c("geocodigoFundar", "geonombreFundar", "sector"),
                                            drop_joined_df = F)


#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df %>%
  select(-valor) %>% 
  argendataR::write_output(
    output_name = output_name,
    subtopico = "CAMCLI",
    control = comparacion,
    fuentes = c(f1, f2),
    analista = "",
    pk = c("geocodigoFundar","sector"),
    es_serie_tiempo = F,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = "geocodigoFundar",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("geocodigoFundar"="codigo geonomenclador",
                                 "geonombreFundar" = "nombre geografico",
                                 anio = "Año",
                                 
                                 "sector" = "Sector", "valor_en_porcent"="Valor de cada sector como porcentaje sobre total"),
    unidades = list("valor_en_porcent" = "Porcentaje de emisiones de CO2"),
    aclaraciones = "Cambio fuente de referencia para el mundo"
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CAMCLI", branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = "CAMCLI",  branch = "main")





