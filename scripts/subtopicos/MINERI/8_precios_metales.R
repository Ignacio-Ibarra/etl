################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "precios_metales"
analista = "Kevin Corfield"
fuente1 <- "R271C141" # Commodity Markets (World Bank)
fuente2 <- "R272C142" # SIACAM precios minerales
# fuente3 <- "R265C0" # IMF Precios commodities Data
# fuente4 <- "R266C0" # IMF Precios commodities Metadata


df_wb <- arrow::read_parquet(argendataR::get_clean_path(fuente1))  

# json_imf_metada <- jsonlite::read_json(argendataR::get_raw_path(fuente4))
# 
# unit_metadata <- json_imf_metada$UNIT_MEASURE %>% 
#   bind_rows() %>%
#   setNames(c("concept_id", "lang", "text", "dimension")) %>%
#   mutate(
#     group = cumsum(concept_id == "UNIT_MEASURE_NAME")
#   ) %>%
#   select(-lang) %>%
#   pivot_wider(
#     id_cols = c(dimension, group),
#     names_from = concept_id,
#     values_from = text
#   ) %>%
#   janitor::clean_names() %>% 
#   select(unit_measure_code, unit_measure_name)
# 
# comm_metadata <- json_imf_metada$COMMODITY %>% 
#   bind_rows() %>%
#   setNames(c("concept_id", "lang", "text", "dimension")) %>%
#   mutate(
#     group = cumsum(concept_id == "COMMODITY_NAME")
#   ) %>%
#   select(-lang) %>%
#   pivot_wider(
#     id_cols = c(dimension, group),
#     names_from = concept_id,
#     values_from = text
#   ) %>%
#   janitor::clean_names() %>% 
#   select(commodity_code, commodity_name, commodity_definition)
# 
# df_imf <- read.csv(argendataR::get_raw_path(fuente3)) %>% 
#   dplyr::filter(freq == "M") %>% 
#   dplyr::filter(unit_measure ==  "USD") %>% 
#   select(-c(freq,ref_area, time_format, unit_measure)) %>% 
#   left_join(comm_metadata, join_by(commodity == commodity_code)) %>% 
#   dplyr::filter(commodity_name == "Lithium")  %>%
#   separate(time_period, into = c("anio", "mes"), sep = "-", convert = TRUE)

df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 

minerales_exportados_wb <- c('Copper', 'Gold', 'Silver')

minerales_exportados_siacam <- c('Carbonato de Litio 99,5% (Li2CO3)')

minerales_nombre = c(
  'Copper' = 'Cobre', 
  'Gold' = 'Oro', 
  'Silver' = 'Plata'
)

df_wb_minerales <- df_wb %>% 
  dplyr::filter(commodity_name %in% minerales_exportados_wb) %>%
  separate(anio_mes, into = c("anio", "mes"), sep = "M", convert = TRUE) %>% 
  mutate(mineral = recode(commodity_name, !!!minerales_nombre)) %>% 
  select(anio, mes, mineral, precio = price_value, unidad_de_medida = price_unit)
  

df_litio <- df_siacam %>% 
  dplyr::filter(mineral %in% minerales_exportados_siacam) %>% 
  select(anio = ano, mes, mineral, precio, unidad_de_medida) %>% 
  mutate(mineral = "Litio")

anio_mes_base <- df_litio %>% distinct(anio, mes) %>% arrange(anio, mes) %>% slice(1)

df_all <- df_wb_minerales %>% 
  bind_rows(df_litio) 

indices_df <- df_all %>% 
  dplyr::filter(anio == anio_mes_base$anio, mes == anio_mes_base$mes) %>% 
  select(mineral, precio_base = precio)

df_output <- df_all %>% 
  group_by(anio, mineral) %>% 
  mutate(anio_completo = n() == 12L) %>% 
  ungroup() %>% 
  dplyr::filter(anio_completo) %>% 
  select(-anio_completo) %>% 
  left_join(indices_df, join_by(mineral)) %>% 
  mutate(indice = 100*precio/precio_base) %>% 
  group_by(anio, mineral) %>% 
  summarise(
    precio = mean(precio, na.rm = T),
    unidad_de_medida = first(unidad_de_medida, n = 1L),
    indice = mean(indice, na.rm = T)
  ) %>% 
  ungroup()







  
# Hago esta transformacion para que el dataset quede anual
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(
    tipo = ifelse(grepl("indice_.*", variable), "indice", "precio"),
    mineral = tools::toTitleCase(str_remove(variable,"indice_")),
    anio = as.numeric(str_extract(fecha, "(\\d{4}).*", group = 1L))
         ) %>% 
  group_by(anio, mineral, tipo) %>% 
  mutate(anio_completo = n() == 12L) %>% 
  ungroup() %>% 
  dplyr::filter(anio_completo) %>% 
  select(-anio_completo) %>% 
  group_by(anio, mineral, tipo) %>% 
  summarise(
    valor = mean(valor, na.rm = T)
  ) %>%
  ungroup() %>% 
  pivot_wider(id_cols = c(anio, mineral), names_from = tipo, values_from = valor) %>% 
  left_join(df_all %>% distinct(mineral, unidad_de_medida), join_by(mineral))



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "mineral"), # variables pk del dataset para hacer el join entre bases
  drop_joined_df =  F
)


#-- Exportar Output ----

armador_descripcion <- function(metadatos, etiquetas_nuevas = data.frame(), output_cols){
  # metadatos: data.frame sus columnas son variable_nombre y descripcion y 
  # proviene de la info declarada por el analista 
  # etiquetas_nuevas: data.frame, tiene que ser una dataframe con la columna 
  # variable_nombre y la descripcion
  # output_cols: vector, tiene las columnas del dataset que se quiere escribir
  
  etiquetas <- metadatos %>% 
    dplyr::filter(variable_nombre %in% output_cols) 
  
  
  etiquetas <- etiquetas %>% 
    bind_rows(etiquetas_nuevas)
  
  
  diff <- setdiff(output_cols, etiquetas$variable_nombre)
  
  stopifnot(`Error: algunas columnas de tu output no fueron descriptas` = length(diff) == 0)
  
  # En caso de que haya alguna variable que le haya cambiado la descripcion pero que
  # ya existia se va a quedar con la descripcion nueva. 
  
  etiquetas <- etiquetas %>% 
    group_by(variable_nombre) %>% 
    filter(if(n() == 1) row_number() == 1 else row_number() == n()) %>%
    ungroup()
  
  etiquetas <- stats::setNames(as.list(etiquetas$descripcion), etiquetas$variable_nombre)
  
  return(etiquetas)
  
}

# Tomo las variables output_name y subtopico declaradas arriba
metadatos <- argendataR::metadata(subtopico = subtopico) %>% 
  dplyr::filter(grepl(paste0("^", output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



etiquetas_nuevas <- data.frame(
  variable_nombre = c("anio",
                      "mineral",
                      "precio",
                      "unidad_de_medida",
                      "indice"),
  descripcion = c("Año de referencia",
                  "Nombre del mineral commodity",
                  "Precio internacional spot en dólares estadounidenses",
                  "Unidad de medida del precio",
                  "Indice del precio nominal (base 100 Noviembre 2018)")
)



descripcion <- armador_descripcion(metadatos = metadatos,
                                   etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)


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
# Usar write_output con exportar = T para generar la salida
# Cambiar los parametros de la siguiente funcion segun su caso


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c("anio", "mineral"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("precio" = "dólares estadounidenses",
                    "indice" = "unidades"),
    aclaracion = "Se modificó el formato del dataset para clarificar las dos variables que son indicadores, se agregaron las filas por año y mineral, se agregó la variable unidad_de_medida para poder conocer cuál es la valuación de cada precio."
  )

