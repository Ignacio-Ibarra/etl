################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_pc_cambio_1975"
analista = "Pablo Sonzogni"
fuente1 <- "R220C0"

geonomenclador <- argendataR::get_nomenclador_geografico() 

# Cargo data desde server
df_wdi <- readr::read_csv(argendataR::get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc=`NY.GDP.PCAP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() %>% 
  dplyr::filter(anio>=1975)


df_1975 <- df_wdi %>% 
  dplyr::filter(anio == 1975) %>% 
  select(iso3, pib_pc_1975 = pib_pc)

df_output <- df_wdi %>% 
  left_join(df_1975, join_by(iso3)) %>% 
  mutate(cambio_relativo = (pib_pc / pib_pc_1975)-1) %>% 
  dplyr::filter(!is.na(cambio_relativo)) %>% 
  select(-pib_pc_1975) %>% 
  left_join(geonomenclador %>% select(geocodigo, name_short), join_by(iso3==geocodigo)) 


check_iso3(df_output$iso3)


# mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = c(fuente1),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    control = comparacion,
    nivel_agregacion = "pais",
    aclaraciones = "El dataset entregado por el analista fue realizado con datos de Maddison Project Database 2020, en cambio en este caso se utilizaron datos de Maddison Project Database 2023. Los países Yugoslavia (YUG), Unión Soviética (SUN) y Checoslovaquia (CSK) fueron incorporados for los autores de la fuente (ver https://onlinelibrary.wiley.com/doi/10.1111/joes.12618). ",
    etiquetas_indicadores = list("pib_pc" = "PBI per cápita (en u$s a precios constantes de 2015)",
                                 "cambio_relativo" = "Tasa de cambio del PBI per cápita de un año con respecto al valor del PBI per cápita de 1975"),
    unidades = list("pib_pc" = "unidades",
                    "cambio_relativo" = "unidades")
  )

output_name <- gsub("\\.csv", "", output_name)

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")
