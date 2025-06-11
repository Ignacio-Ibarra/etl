#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "30_share_global_expo_pesca_pais_anio.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R397C0' # Atlas SITC

df_sitc <- argendataR::get_raw_path(fuente1) %>%
  read_csv()


geonomenclador <- argendataR::get_nomenclador_geografico_front() %>%
  select(iso3 = geocodigo, pais_nombre = name_long)

df_output <- df_sitc %>% 
  dplyr::filter(grepl("^03.*", product_sitc_code)) %>% 
  group_by(anio = year, iso3 = country_iso3_code) %>% 
  summarise(
    expo_pesca = sum(export_value)
  ) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(
    share_global = 100*expo_pesca / sum(expo_pesca, na.rm = T)
  ) %>% 
  ungroup() %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  mutate(
    pais_nombre = ifelse(iso3 == "YUG", "Yugoslavia", pais_nombre)
  )



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c('anio','iso3')

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
    columna_geo_referencia = 'iso3',
    nivel_agregacion = NULL
  )