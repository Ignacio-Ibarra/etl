################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

#-- Descripcion ----
#' Breve descripcion de output creado
#'
subtopico <- "INFDES"
output_name <- "tasa_formalidad_productiva_pib_per_capita"
fuente1 <- "R115C31"
fuente2 <- "R126C0"
#-- Librerias ----

#-- Lectura de Datos ----

# Los datos a cargar deben figurar en el script "fuentes_SUBTOP.R" 
# Se recomienda leer los datos desde tempdir() por ej. para leer maddison database codigo R37C1:
inf_df <- readr::read_csv(argendataR::get_temp_path(fuente1))
gdp_df <- readr::read_csv(argendataR::get_temp_path(fuente2)) %>% 
  rename(c("pib_per_capita_ppp" = "NY.GDP.PCAP.PP.KD")) %>% 
  select(anio = year, iso3 = iso3c, pib_per_capita_ppp)


formalidad_df <- inf_df %>% 
  dplyr::filter((serie == "Serie original") & (apertura == "Edad total")) %>% 
  select(-one_of("topico", "variable", "serie", "apertura"), everything()) %>% 
  group_by(pais, tematica) %>% 
  mutate(dummy_anio_max = max(anio) == anio) %>% 
  ungroup() %>% 
  dplyr::filter(dummy_anio_max) %>% 
  mutate(tasa_formalidad_productiva = 1 - valor/100) %>% 
  select(iso3, pais, anio, tasa_formalidad_productiva) %>% 
  dplyr::filter(anio>2012)
  

df_output <- formalidad_df %>% 
  left_join(gdp_df, by = join_by(anio, iso3))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df_output,
  nombre = output_name,
  pk = c("anio", "iso3"),
  drop_joined_df = F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = "",
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = "Este dataset cuenta con un cambio. Dado que el Banco Mundial actualizó el año base (de 2017 a 2021) para el cálculo del indicador 'NY.GDP.PCAP.PP.KD' el cual es utilizado en esta fuente",
    etiquetas_indicadores = list("tasa_formalidad_productiva" = "Tasa de formalidad (definción productiva)",
                                 "pib_per_capita_ppp" = "GDP per capita, PPP (constant 2021 international $)"),
    unidades = list("tasa_formalidad_productiva" = "unidades",
                    "pib_per_capita_ppp" = "unidades")
  )

