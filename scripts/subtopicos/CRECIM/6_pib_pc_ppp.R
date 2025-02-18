################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_pc_ppp_2017"
analista = "Pablo Sonzogni"
fuente1 <- "R126C0"


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion) 
  


# Cargo data desde server
data_pibpc_ppp <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc =`NY.GDP.PCAP.PP.KD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 


df_output <- data_pibpc_ppp #%>%
#   left_join(geonomenclador, join_by(iso3)) %>%
#   dplyr::filter(nivel_agregacion!="agregacion")



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
    control = comparacion,
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = "Respecto al dataset entregado por el analista este dataset posee un cambio sustancial debido a que ha variado el dato en la fuente ya que el Banco Mundial ha modificado el año base de 2017 a 2021",
    etiquetas_indicadores = list("pib_pc" = "PBI per cápita PPA (en u$s a precios internacionales constantes de 2021)"),
    unidades = list("pib_pc" = "unidades")
  )


mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")

