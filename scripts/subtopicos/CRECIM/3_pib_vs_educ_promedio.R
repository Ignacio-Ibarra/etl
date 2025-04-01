################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_vs_educ_promedio"
analista = "Pablo Sonzogni"
fuente1 <- "R126C0"
fuente2 <- "R216C87"


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}
get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}





# Cargo data desde server
data_pibpc_ppp <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc =`NY.GDP.PCAP.PP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 


data_mys <- arrow::read_parquet(get_clean_path(fuente2)) %>% rename(anios_escol_prom = mean_years_schoolling)

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion, es_iso) %>% 
  dplyr::filter(es_iso == 1) %>% 
  select(-es_iso)


df_output <- data_pibpc_ppp %>% 
  left_join(data_mys, by=join_by(iso3, anio)) %>% 
  dplyr::filter(!is.na(anios_escol_prom)) %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  left_join(geonomenclador, join_by(iso3))

df_output <- df_output %>% 
  select(-c(pais_nombre, continente_fundar, nivel_agregacion))

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  rename(pib_pc = pib_percapita_ppp_2017) 
  


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior %>% 
    select(-c(pais_nombre, continente_fundar, nivel_agregacion)),
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
    aclaraciones = "La version anterior usaba PBI per cápita PPA en u$s a precios constantes internacionales de 2017. Se actualizo usando u$s a precios constantes internacionales de 2021. Entre una versión 2023 y 2024 del Human Development Report, los países Antigua y Barbuda, Benin, Burkina Faso, Camerún, Congo, Cabo Verde, Djibouti, Etiopía, Micronesia, Ghana, Guinea, Guinea Ecuatorial, Granada, Saint Kitts y Nevis, Lesotho, Malí, Malawi, Níger y Sierra Leona poseen diferencias elevadas con respecto a la variable referida a los años promedio de educación",
    etiquetas_indicadores = list("pib_pc" = "PBI per cápita PPA (en u$s a precios internacionales constantes de 2021)",
                                 "anios_escol_prom" = "Promedio de años de escolaridad alcanzados"),
    unidades = list('pib_pc' = "u$s a precios constantes internacionales de 2021 / hab",
                    'anios_escol_prom' = "cantidad de años promedio de escolaridad")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")



