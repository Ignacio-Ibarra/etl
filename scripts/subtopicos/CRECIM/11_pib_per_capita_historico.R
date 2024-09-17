################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "pib_per_capita_historico.csv"
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

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, continente_fundar, nivel_agregacion) 


# Cargo data desde server
df_madd_c <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  select(anio, iso3, area_desc = pais_nombre, gdppc) 

df_madd_r <- arrow::read_parquet(get_clean_path(fuente2)) %>%
  select(anio, iso3, area_desc = region, gdppc)

df_all <- df_madd_c %>% 
  bind_rows(df_madd_r) %>% 
  dplyr::filter(!is.na(gdppc))


df_output <- df_all %>% 
  rename(pib_per_capita = gdppc) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion))


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
    fuentes = c(fuente1, fuente2),
    analista = analista,
    pk = c("anio", "iso3"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "iso3",
    nivel_agregacion = "pais",
    etiquetas_indicadores = list("pib_per_capita" = "PBI per cápita PPA (en u$s a precios internacionales constantes de 2011)"),
    unidades = list("pib_per_capita" = "unidades")
  )
