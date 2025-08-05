################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_vs_ingreso"
analista = "Pablo Sonzogni"
fuente1 <- "R126C0" # GDP per capita, PPP (current international $)
fuente2 <- "R217C89" # Poverty and Inequality Platform


# Cargo data desde server
data_pibpc_ppp <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc =`NY.GDP.PCAP.PP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 

# Dado que Argentina no tiene "nacional" elijo las filas donde tengo Argentina (urbana) o las filas del resto
# de los países donde el reporting level es "nacional"
data_ing <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  dplyr::filter((iso3 == "ARG"  & reporting_level == "urban") | (reporting_level == "national" & iso3 != "ARG")) %>% 
  select(iso3, anio, medida_bienestar = welfare_type, reporting_level, poverty_line, promedio = mean) %>% 
  dplyr::filter(!is.na(promedio)) %>% 
  mutate(medida_bienestar = ifelse(medida_bienestar == "consumption", "consumo", "ingreso"))

geonomenclador <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigoFundar = geocodigo, geonombreFundar = name_long)
  

df_output <- data_ing %>% 
  rename(geocodigoFundar = iso3) %>% 
  left_join(data_pibpc_ppp %>% rename(geocodigoFundar = iso3), join_by(geocodigoFundar, anio)) %>% 
  drop_na(pib_pc) %>% 
  left_join(geonomenclador, join_by(geocodigoFundar)) %>% 
  select(geocodigoFundar, geonombreFundar, anio, medida_bienestar, reporting_level, poverty_line, promedio, pib_pc)

check_iso3(df_output$geocodigoFundar)


#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


df_anterior <- argendataR::descargar_output(nombre = "pib_vs_ingreso.csv", subtopico = subtopico, branch = "main")



comparacion <- argendataR::comparar_outputs(
  df_output %>% select(-c(reporting_level, poverty_line)),
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("geocodigoFundar", "anio", "medida_bienestar"), 
  drop_joined_df = F
)

df_output %>%
  distinct(geocodigoFundar, anio) %>% 
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
    pk = c("anio", "geocodigoFundar"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "geocodigoFundar",
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

