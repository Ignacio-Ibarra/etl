#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "PESCAS"
output_name <- "28_consumo_pescado_decil.csv"
analista <- "Ignacio Ibarra"
fuente1 <- 'R394C248' # ENGHO 2017/2018 Base de hogares
fuente2 <- 'R392C245' # ENGHO 2017/2018 Base de gasto
fuente3 <- 'R393C247' # ENGHO 2017/2018 Articulos


df_hogares <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet() %>% 
  select(id, dinth_t) %>% 
  mutate(dinth_t = as.integer(dinth_t))


df_gasto <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() 


df_articulos <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet()

df_alimentos <- df_articulos %>% 
  dplyr::filter(grepl("^A011.*", articulo)) %>% 
  select(articulo, articulo_desc, clase_desc, subclase_desc, grupo_desc)


df_gasto_alimentos <- df_gasto %>% 
  right_join(df_alimentos, join_by(articulo)) 


df_gasto_deciles <- df_hogares %>% 
  left_join(df_gasto_alimentos, join_by(id)) %>% 
  mutate(pondera = ifelse(is.na(pondera),0, pondera),
         monto = ifelse(is.na(monto), 0, monto),
         ponderamonto = pondera*monto) %>% 
  group_by(articulo, articulo_desc, clase_desc, subclase_desc, grupo_desc, dinth_t) %>% 
  summarise(
    ponderamonto = sum(ponderamonto, na.rm = T)
  ) %>% 
  ungroup() %>% 
  group_by(dinth_t) %>% 
  mutate(
    share_gasto = round(100* ponderamonto / sum(ponderamonto),3)
  ) %>% 
  ungroup() %>% 
  dplyr::filter(clase_desc == "Pescado") %>% 
  select(-ponderamonto)


df_output <- df_gasto_deciles %>% 
  group_by(dinth_t, subclase_desc) %>% 
  summarise(
    share_gasto = sum(share_gasto)
  ) %>% 
  ungroup()



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))


pk <- c('dinth_t', 'subclase_desc')

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
    es_serie_tiempo = F,
    control = comparacion,
    columna_indice_tiempo = NULL,
    columna_geo_referencia = NULL,
    nivel_agregacion = NULL,
  )