################################################################################
###                             Dataset: nombre                              ###
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection
require(WDI)
library(sjlabelled)

subtopico <- "CRECIM"
output_name <- "pib_absoluto_per_capita"
analista = "Pablo Sonzogni"
fuente1 <- "R214C0"
fuente2 <- "R213C0"

#-- Librerias ----

get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}




# Descargo data usando wrapper https://github.com/vincentarelbundock/WDI
data_pibpc <- read_csv(get_raw_path(fuente1)) %>% 
  select(iso3 = iso3c, anio = year, pib_pc=`NY.GDP.PCAP.CD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib_pc)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 
  # dplyr::filter(!is.na(iso3) && !is.na(pib_pc) && iso3 != "")


# Descargo data usando wrapper https://github.com/vincentarelbundock/WDI
data_pib <-read_csv(get_raw_path(fuente2)) %>% 
  select(iso3 = iso3c, anio = year, pib=`NY.GDP.MKTP.CD`) %>% 
  dplyr::filter(iso3!="") %>% 
  dplyr::filter(!is.na(pib)) %>% 
  dplyr::filter(!is.na(iso3)) %>% 
  sjlabelled::zap_labels() 
  # dplyr::filter(!is.na(iso3) && !is.na(pib) && iso3!="")


geonomenclador <- argendataR::get_nomenclador_geografico() %>%
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, es_iso)

data <- data_pib %>% 
  left_join(data_pibpc , by= join_by(iso3, anio)) %>% 
  left_join(geonomenclador, by = join_by(iso3)) %>% 
  dplyr::filter(es_iso == 1) %>% 
  dplyr::filter(!(pais_nombre %in% c("Isla de Man", "Macao", "Islas Caimán", "Bermuda", "Islas Feroe"))) %>% 
  select(-es_iso) %>% 
  group_by(anio) %>% mutate(ranking_pib = dense_rank(desc(pib)),
                       ranking_pib_pc = dense_rank(desc(pib_pc))) %>% 
  ungroup()


df_comparar <- data %>% 
  filter(anio == 2021) %>% 
  select(-anio, -pib, -pib_pc) %>% 
  mutate(ranking_pib = as.numeric(ranking_pib),
         ranking_pib_pc = as.numeric(ranking_pib_pc))
 
  
df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") 

#-- Controlar Output ----

# Usar la funcion comparar_outputs para contrastar los cambios contra la version cargada en el Drive
# Cambiar los parametros de la siguiente funcion segun su caso


comparacion <- argendataR::comparar_outputs(
  df = df_comparar,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df = F
)

#-- Procesamiento ----

df_output <- data %>% 
  filter(anio == max(anio)) %>% 
  select(-anio, -pib, -pib_pc) %>% 
  mutate(ranking_pib = as.numeric(ranking_pib),
         ranking_pib_pc = as.numeric(ranking_pib_pc))



#-- Exportar Output ----

# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = c(fuente1, fuente2),
    analista = analista,
    pk = c("iso3"),
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = glue::glue("Los datos corresponden al año {max(data$anio)}"),
    etiquetas_indicadores = list("ranking_pib" = "Ranking que ocupa según el PIB"),
    unidades = list("ranking_pib_pc" = "Ranking que ocupa según el PIB per capita")
  )

