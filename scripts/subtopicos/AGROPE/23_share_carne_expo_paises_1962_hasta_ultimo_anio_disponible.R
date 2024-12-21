################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "share_carne_expo_paises_seleccionados_1962_hasta_ultimo_anio_disponible"
output_name_old <- "share_carne_expo_paises_seleccionados_1962_2021"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R302C170" # Atlas location
fuente2 <- "R301C169" # Atlas exports and import by country SITC2 product  656 Bovine meat, fresh, chilled or frozen


df_countries <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) %>% 
  select(m49_code, iso3)

df_expo_carne <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) %>% 
  select(m49_code = country_id, anio = year, export_value)

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais = desc_fundar)

df_output <- df_expo_carne %>% 
  group_by(anio) %>% 
  mutate(
    share_expo = export_value / sum(export_value)
  ) %>% 
  ungroup() %>% 
  left_join(df_countries, join_by(m49_code)) %>% 
  left_join(geonomenclador, join_by(iso3)) %>% 
  select(anio, iso3, pais, share_expo)




df_anterior <- argendataR::descargar_output(nombre = output_name_old,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('iso3', 'anio'), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0("^", output_name_old,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("share_expo"),
  descripcion = c("Participación en las exportaciones de 'Carne de bovino, fresca, refrigerada o congelada', código SITC Rev 2 0111")
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
    pk = c("iso3","anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    cambio_nombre_output = list("nombre_nuevo" = output_name, 'nombre_anterior' = output_name_old),
    unidades = list("share_expo" = "porcentaje"),
    aclaraciones = "Se agregan varios países y años que antes no estaban, los test de la comparación no los pasas, pero el qqplot está bastante bien"
  )


