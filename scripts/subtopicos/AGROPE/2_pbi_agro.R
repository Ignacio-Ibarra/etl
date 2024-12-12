################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "pbi_agro"
analista = "Franco Antonio Mendoza"
fuente1 <- "R36C82"
fuente2 <- "R223C93"

# Fuentes repetidas
# A <- arrow::read_parquet(argendataR::get_clean_path("R36C73"))
# B <- arrow::read_parquet(argendataR::get_clean_path("R36C74"))
# C <- arrow::read_parquet(argendataR::get_clean_path("R36C82"))
  

vabpb_fnys <- arrow::read_parquet(argendataR::get_clean_path(fuente1))


vabpb_indec <- arrow::read_parquet(argendataR::get_clean_path(fuente2))




# Procesamiento -------

vabpb_agro_fnys <- vabpb_fnys %>% 
  dplyr::filter(unidad == 'mill. de $ 2004') %>% 
  dplyr::filter(indicador == "Agricultura, ganadería, caza y silvicultura") %>% 
  select(anio, vab_pb_agro_pcons_fnys = valor)

  


# me quedo solo con los datos de total anual
vabpb_agro_indec <- vabpb_indec %>% 
  dplyr::filter(trimestre == "Total") %>%
  dplyr::filter(letra == "A") %>% 
  dplyr::filter(sub_sector == "Total sector") %>% 
  select(anio, vab_pb_agro_pcons_indec = vab_pb)
  

df_output <- vabpb_agro_fnys %>% 
  full_join(vabpb_agro_indec, join_by(anio)) %>% 
  mutate(
    vab_pb_agro_pcons = ifelse(anio < 2004, vab_pb_agro_pcons_fnys, vab_pb_agro_pcons_indec)
  ) %>% 
  select(anio, vab_pb_agro_pcons)

df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(
    vab_pb_agro_pcons = pbi_agro_pcons
  )


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio"), 
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
                      "vab_pb_agro_pcons"),
  descripcion = c("Año de referencia",
                  "Valor Agregado Bruto de 'Agricultura, caza y silvicultura' a precios básicos en en millones pesos constantes de 2004")
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
    pk = c("anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("vab_pb_agro_pcons" = "millones de pesos constantes de 2004")
  )

