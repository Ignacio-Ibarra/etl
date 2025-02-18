################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_corriente_constante"
analista = "Pablo Sonzogni"
fuente1 <- "R218C0"
fuente2 <- "R213C0"


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}



# Cargo data desde server
data_gdp_kd <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_constante =`NY.GDP.MKTP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels()

data_gdp_cd <- read_csv(get_raw_path(fuente2)) %>% 
  select(iso3 = iso3c, anio = year, pib_corriente =`NY.GDP.MKTP.CD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 



geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) 


df_output <- data_gdp_cd %>% 
  left_join(data_gdp_kd, by=join_by(iso3, anio)) %>% 
  dplyr::filter(!(is.na(pib_constante) | is.na(pib_corriente)) )
  

#-- Controlar Output ----

check_iso3(df_output$iso3)

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")



comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "anio"), 
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
    analista = analista,
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("pib_constante" = "PBI (en u$s a precios constantes de 2015)",
                                 "pib_corriente" = "PBI (en u$s a precios corrientes)"),
    unidades = list('pib_constante' = "unidades",
                    'pib_corriente' = "unidades")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")


