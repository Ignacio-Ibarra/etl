#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "16_valor_cantidad_precio_exportacion_pesquero.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R319C188' # INDEC_COMEX


df_indec <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.)


pescado <- c("03","1504","1604","1605","230120", "05080000", "051191","16030090") #obtenidos de https://www.indec.gob.ar/ftp/cuadros/economia/nota_metodologica_complejos_exportadores_2024.pdf

pattern_pescado <- paste0("^(", paste(pescado, collapse = "|"), ")")


df_output <- df_indec %>% 
  drop_na(fob) %>% 
  dplyr::filter(grepl(pattern_pescado, ncm8)) %>%
  group_by(anio) %>% 
  summarise(
    toneladas = sum(pnet_kg, na.rm = T)/1000,
    fob = sum(fob, na.rm = T),
    precio = sum(fob, na.rm = T) / sum(pnet_kg, na.rm = T)/1000
  ) %>% 
  ungroup() %>% 
  mutate(
    fob_index = (fob / fob[anio == 2002]) * 100,
    toneladas_index = (toneladas / toneladas[anio == 2002]) * 100,
    precio_index = (precio / precio[anio == 2002]) * 100
  ) %>% 
  select(-toneladas, -precio, -fob)



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
  
