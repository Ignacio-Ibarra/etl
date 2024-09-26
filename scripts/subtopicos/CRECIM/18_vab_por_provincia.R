################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "CRECIM"
output_name <- "vab_por_provincia"
analista = "Pablo Sonzogni"
fuente1 <- "R221C92"


geoAr::get_provincias(nombre="Tucumán")


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
df_output <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(sector_de_actividad_economica == "Total sectores") %>% 
  select(provincia_id, provincia_nombre = provincia, region, anio, vab_pb) %>% 
  group_by(anio) %>% 
  mutate(participacion = vab_pb / sum(vab_pb, na.rm = T)) %>% 
  ungroup()



df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "provincia_nombre"), # variables pk del dataset para hacer el join entre bases
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
    pk = c("anio", "provincia_id"),
    es_serie_tiempo = T,
    columna_indice_tiempo = "anio",
    columna_geo_referencia = "provincia_id",
    nivel_agregacion = "provincia",
    aclaraciones = "Este dataset a diferencia del analista posee pequeñas diferencias dado que el analista no contemplo la parte del VAB que no se distribuyó entre provincias",
    etiquetas_indicadores = list("vab_pb" = "Valor Agregado Bruto a precios básicos, en millones de pesos a precios de 2004",
                                 "participacion" = "Participacion de la provincia en el VAB nacional"),
    unidades = list("vab_pb" = "Millones de pesos a precios de 2004",
                    "participacion" = "unidades")
  )
