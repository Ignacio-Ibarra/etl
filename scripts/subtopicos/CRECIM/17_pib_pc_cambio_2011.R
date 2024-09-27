################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_pc_cambio_2011"
analista = "Pablo Sonzogni"
fuente1 <- "R220C0"



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

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, area_desc = desc_fundar, continente_fundar, nivel_agregacion) 


# Cargo data desde server
df_wdi <- readr::read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc=`NY.GDP.PCAP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() %>% 
  dplyr::filter(anio>=2011)


df_2011 <- df_wdi %>% 
  dplyr::filter(anio == 2011) %>% 
  select(iso3, pib_pc_2011 = pib_pc)

df_output <- df_wdi %>% 
  left_join(df_2011, join_by(iso3)) %>% 
  mutate(cambio_relativo = (pib_pc / pib_pc_2011)-1) %>% 
  dplyr::filter(!is.na(cambio_relativo)) %>% 
  select(-pib_pc_2011,-pib_pc) %>% 
  left_join(geonomenclador, join_by(iso3)) 


# mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


metadata_17 <- argendataR::metadata("CRECIM") %>% filter(str_detect(dataset_archivo, output_name))



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
    aclaraciones = "El dataset entregado por el analista fue realizado con datos de Maddison Project Database 2020, en cambio en este caso se utilizaron datos de Maddison Project Database 2023. Los países Yugoslavia (YUG), Unión Soviética (SUN) y Checoslovaquia (CSK) fueron incorporados for los autores de la fuente (ver https://onlinelibrary.wiley.com/doi/10.1111/joes.12618). ",
    etiquetas_indicadores = list("pib_pc" = "PBI per cápita (en u$s a precios constantes de 2015)",
                                 "cambio_relativo" = "Tasa de cambio del PBI per cápita de un año con respecto al valor del PBI per cápita de 2011"),
    unidades = list("pib_pc" = "unidades",
                    "cambio_relativo" = "unidades")
  )
