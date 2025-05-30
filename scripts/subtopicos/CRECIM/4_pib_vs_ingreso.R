################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_vs_ingreso"
analista = "Pablo Sonzogni"
fuente1 <- "R126C0"
fuente2 <- "R217C89"


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
  sjlabelled::zap_labels() %>% 
  mutate(iso3 = ifelse(iso3 == "XKX", "KOS", iso3))


# Dado que Argentina no tiene "nacional" elijo las filas donde tengo Argentina (urbana) o las filas del resto
# de los países donde el reporting level es "nacional"
data_ing <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  dplyr::filter((iso3 == "ARG"  & reporting_level == "urban") | (reporting_level == "national" & iso3 != "ARG"))

data_ing <- data_ing %>% 
  select(iso3, anio, welfare_type, mean, reporting_level, reporting_gdp)

# geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
#   select(iso3 = codigo_fundar, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) 
  

df_output <- data_ing %>% 
  dplyr::filter(!is.na(mean)) %>% 
  # dplyr::filter(!is.na(pib_pc)) %>% 
  # left_join(geonomenclador, join_by(iso3)) %>% 
  rename(medida_bienestar = welfare_type,
         promedio = mean,
         pib_pc = reporting_gdp)

df_output <-  df_output %>% 
  select(-c(reporting_level)) %>% 
  mutate(medida_bienestar = ifelse(medida_bienestar == "consumption", "consumo", "ingreso"))

check_iso3(df_output$iso3)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(nombre = "pib_vs_ingreso.csv", subtopico = subtopico, branch = "main") %>% 
  rename(pib_pc = pib_percapita_ppp_2017)



comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior %>% 
    select(-c(pais_nombre, continente_fundar, nivel_agregacion)),
  nombre = output_name,
  pk = c("iso3", "anio", "medida_bienestar"), 
  drop_joined_df = F
)

df_output %>%
  distinct(iso3, anio) %>% 
  count(anio) %>% 
  arrange(-anio)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1, fuente2),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    control = comparacion,
    aclaraciones = "La version anterior usaba PBI per cápita PPA en u$s a precios constantes internacionales de 2017. Se actualizo usando u$s a precios constantes internacionales de 2021.",
    etiquetas_indicadores = list("pib_pc" = "PBI per cápita PPA (en u$s a precios internacionales constantes de 2021) / hab.",
                                 "promedio" = "Ingreso/consumo per cápita diario PPA (en u$s a percios internacionales constantes de 2021)"),
    unidades = list('pib_pc' = "unidades",
                    'promedio' = "unidades")
  )

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")


