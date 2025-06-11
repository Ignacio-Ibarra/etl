#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "15_participacion_complejo_pesquero_en_expo.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R396C0' # Atlas Services
fuente2 <- 'R397C0' # Atlas SITC

df_services<- argendataR::get_raw_path(fuente1) %>%
  read_csv()

df_sitc <- argendataR::get_raw_path(fuente2) %>%
  read_csv()

df_sitc_arg <- df_sitc %>% 
  dplyr::filter(country_iso3_code == "ARG")

df_sitc_arg_pesca <- df_sitc_arg %>% 
  dplyr::filter(grepl("^03.*", product_sitc_code)) %>% 
  group_by(anio = year) %>% 
  summarise(
    total_pesca = sum(export_value)
  ) %>% 
  ungroup()

df_sitc_bienes_arg <- df_sitc_arg %>% 
  group_by(anio = year) %>% 
  summarise(
    total_bienes = sum(export_value)
  ) %>% 
  ungroup()
  

df_services_arg <- df_services %>% 
  dplyr::filter(country_iso3_code == "ARG") %>% 
  group_by(anio = year) %>% 
  summarise(
    total_servicios = sum(export_value)
  ) %>% 
  ungroup()


df_output <- df_sitc_bienes_arg %>% 
  left_join(df_sitc_arg_pesca, join_by(anio)) %>% 
  left_join(df_services_arg, join_by(anio)) %>% 
  mutate(share_bienes = 100*total_pesca / total_bienes,
         share_expo = 100*total_pesca / (total_bienes + total_servicios)) %>% 
  select(anio, share_bienes, share_expo) %>%  
  drop_na(share_bienes)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))

pk <- c("anio")

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
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )

