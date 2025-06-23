# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "PESCAS"
output_name <- "22_pbg_pesquero.csv"
analista <- "Ignacio Ibarra"

# Defino las fuentes
fuente1 <- 'R221C92' # CEPAL Desagregación provincial del valor agregado bruto de la Argentina, base 2004


df_cepal <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


geo_info <- argendataR::get_nomenclador_geografico_front() %>% 
  dplyr::filter(grepl("^AR-\\w$", geocodigo)) %>% 
  select(geocodigo, provincia = name_long)

df_output <- df_cepal %>% 
  dplyr::filter(sector_de_actividad_economica == "Pesca") %>% 
  group_by(anio) %>% 
  mutate(share = 100*vab_pb / sum(vab_pb, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(provincia_id,anio) %>% 
  dplyr::filter(vab_pb >0) %>% 
  ungroup() %>% 
  select(-sector_de_actividad_economica, -provincia_id, -region) %>% 
  left_join(geo_info, join_by(provincia))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c("anio", "geocodigo")

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = pk, # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)

colectar_fuentes <- function(pattern = "^fuente.*"){
  
  # Genero un vector de codigos posibles
  posibles_codigos <- c(fuentes_raw()$codigo,fuentes_clean()$codigo)
  
  # Usar ls() para buscar variables en el entorno global
  variable_names <- ls(pattern = pattern, envir = globalenv())
  
  # Obtener los valores de esas variables
  valores <- unlist(mget(variable_names, envir = globalenv()))
  
  # Filtrar aquellas variables que sean de tipo character (string)
  # Esto es para que la comparacion sea posible en la linea de abajo
  strings <- valores[sapply(valores, is.character)]
  
  # solo devuelvo las fuentes que existen
  return(valores[valores %in% posibles_codigos])
}



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk =  pk,
    es_serie_tiempo = T,
    control = comparacion,
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'geocodigo',
    nivel_agregacion = 'provincia',
  )