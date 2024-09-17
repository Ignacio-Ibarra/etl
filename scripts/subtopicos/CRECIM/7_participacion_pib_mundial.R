################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "participacion_pib_mundial"
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
  dplyr::filter(anio>=1820) %>% 
  select(anio, iso3, area_desc = pais_nombre, pib) 

df_madd_r <- arrow::read_parquet(get_clean_path(fuente2)) %>%
  select(anio, iso3, area_desc = region, pib)


pib_mundial <- df_madd_r %>% dplyr::filter(iso3 == "WLD") %>% 
  select(anio, pib_mundial = pib)

df_output <- df_madd_c %>% 
  bind_rows(df_madd_r) %>%
  dplyr::filter(iso3 != "WLD") %>%
  left_join(pib_mundial, join_by(anio)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(participacion = pib / pib_mundial) %>% 
  mutate(nivel_agregacion = ifelse(is.na(nivel_agregacion), "agregacion", nivel_agregacion)) %>% 
  select(-pib, -pib_mundial) %>% 
  dplyr::filter(!is.na(participacion))
  
  
  
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
    aclaraciones = "El dataset entregado por el analista fue realizado con datos de Maddison Project Database 2020, en cambio en este caso se utilizaron datos de Maddison Project Database 2023. Los países Yugoslavia (YUG), Unión Soviética (SUN) y Checoslovaquia (CSK) fueron incorporados for los autores de la fuente (ver https://onlinelibrary.wiley.com/doi/10.1111/joes.12618). Por dicho motivo la suma de las participaciones para los años 1950 en adelante son mayores a la unidad. En cambio para los años anteriores, la suma de las participaciones son inferiores a la unidad, dado que se cuenta con una estimación del pib mundial pero no se cuenta con estimación para muchos países. De todos modos se considera que el calculo obtenido para cada país es válido",
    etiquetas_indicadores = list("participacion" = "Participación en el PBI PPA (en u$s a precios internacionales constantes de 2011)"),
    unidades = list("participacion" = "unidades")
  )

