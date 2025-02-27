################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "participacion_pib_latam"
analista = "Pablo Sonzogni"
fuente1 <- "R219C90"
fuente2 <- "R219C91"


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

geonomenclador <- argendataR::get_nomenclador_geografico() 


# Cargo data desde server
df_madd_latam <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(anio>=1820) %>% 
  dplyr::filter(region == "América Latina (Maddison Project Database)") %>% 
  select(anio, iso3, pib) 

df_madd_r <- arrow::read_parquet(get_clean_path(fuente2)) %>%
  select(anio, iso3, pib)


pib_latam <- df_madd_r %>% dplyr::filter(iso3 == "LAC_MPD") %>% 
  select(anio, pib_latam = pib)

df_no_resto <- df_madd_latam %>% 
  left_join(pib_latam, join_by(anio)) %>% 
  # left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(participacion = pib / pib_latam) %>% 
  select(-pib, -pib_latam) %>% 
  dplyr::filter(!is.na(participacion)) %>% 
  dplyr::filter(iso3 %in% c("BRA","ARG","MEX"))

df_resto <- df_no_resto %>% 
  group_by(anio) %>% 
  summarise(participacion = 1 - sum(participacion, na.rm = T)) %>% 
  ungroup() %>% 
  mutate(iso3 = "CRECIM_AML-RESTO",
         pais_nombre = "Resto de América Latina")
  
df_output <- df_no_resto %>% bind_rows(df_resto) %>% 
  select(-pais_nombre)

check_iso3(df_output$iso3)

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior %>% 
    mutate(iso3 = replace_na(iso3, "CRECIM_AML-RESTO")),
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
    fuentes = c(fuente1, fuente2),
    analista = analista,
    control = comparacion,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    aclaraciones = "El dataset entregado por el analista fue realizado con datos de Maddison Project Database 2020, en cambio en este caso se utilizaron datos de Maddison Project Database 2023",
    etiquetas_indicadores = list("participacion" = "Participación en el PBI PPA (en u$s a precios internacionales constantes de 2011) con respecto a Latino América"),
    unidades = list("participacion" = "unidades")
  )


output_name <- gsub("\\.csv", "", output_name)

mandar_data(paste0(output_name, ".csv"), subtopico = "CRECIM", branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = "CRECIM",  branch = "dev")

