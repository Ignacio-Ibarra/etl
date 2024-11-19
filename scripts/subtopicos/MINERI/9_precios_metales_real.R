################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "precios_metales_real"
analista = "Kevin Corfield"
fuente1 <- "R271C141" # Commodity Markets (World Bank)
fuente2 <- "R272C142" # SIACAM precios minerales
fuente3 <- "R273C0" # Consumer Price Index St. Louis Fed


df_wb <- arrow::read_parquet(argendataR::get_clean_path(fuente1))  

df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 


df_cpi <- readr::read_csv(argendataR::get_raw_path(fuente3)) %>% select(date, cpi_value = value) %>% 
  mutate(anio = year(as.Date(date)),
         mes = lubridate::month(as.Date(date)),
         date = as.Date(date)) 

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

anio_mes_base_litio <- df_litio %>% distinct(anio, mes) %>% arrange(anio, mes) %>% slice(1)

fecha_base_cpi <- "2023-12-01"

df_cpi_base_Dic2023 <- df_cpi %>% 
  mutate(cpi_coef = cpi_value / df_cpi$cpi_value[which(df_cpi$date == as.Date(fecha_base_cpi))]) %>% 
  select(-date)


df_all <- df_wb_minerales %>% 
  bind_rows(df_litio) %>% 
  left_join(df_cpi_base_Dic2023, join_by(anio, mes))


indices_df <- df_all %>% 
  dplyr::filter(anio == anio_mes_base_litio$anio, mes == anio_mes_base_litio$mes) %>% 
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
    cpi_coef = mean(cpi_coef, na.rm = T),
    precio_nominal = mean(precio, na.rm = T),
    unidad_de_medida = first(unidad_de_medida, n = 1L),
    indice_nominal = mean(indice, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(precio_real = precio_nominal / cpi_coef,
         indice_real = indice_nominal / cpi_coef) %>% 
  select(anio, mineral, precio_real, indice_real, unidad_de_medida)


# Hago esta transformacion para que el dataset quede anual
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  dplyr::filter(grepl(".*_real", variable)) %>% 
  mutate(
    tipo = ifelse(grepl("indice_.*", variable), "indice_real", "precio_real"),
    mineral = tools::toTitleCase(str_extract(variable,".*(oro|litio|plata|cobre)_real", group = 1)),
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
                      "precio_real",
                      "unidad_de_medida",
                      "indice_real"),
  descripcion = c("Año de referencia",
                  "Nombre del mineral commodity",
                  "Precio internacional spot en dólares estadounidenses, a precios constantes de 2023",
                  "Unidad de medida del precio",
                  "Indice del precio nominal (base 100 Noviembre 2018), a precios constantes de 2023")
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
    unidades = list("precio_real" = "dólares estadounidenses de 2023",
                    "indice_real" = "unidades"),
    aclaracion = "Se modificó el formato del dataset para clarificar las dos variables que son indicadores, se agregaron las filas por año y mineral, se agregó la variable unidad_de_medida para poder conocer cuál es la valuación de cada precio. Se utilizó el índice de precios al consumidor de Estados Unidos de 2023 para cambiar el año base, y se utilizó dicho índice para calcular el precio real y el indice real de los minerales"
  )

