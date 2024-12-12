################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "apertura_cuentas_actividad_agropecuaria"
analista = "Franco Antonio Mendoza"
fuente1 <- "R223C94" # Valor Agregado Bruto a precios básicos por rama de actividad económica. Valores anuales en millones de pesos a precios corrientes



df_vabpb <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) %>% 
  dplyr::filter(trimestre == "Total")


df_vab_total <- df_vabpb %>% 
  dplyr::filter(sub_sector == "Total sector") %>% 
  group_by(anio) %>% 
  summarise(
    vab_pb_total = sum(vab_pb, na.rm = T)
  ) %>% 
  ungroup()

df_vab_agro <- df_vabpb %>% 
  dplyr::filter(letra == "A") %>% 
  mutate(
    cuenta = case_when(
      sub_sector == "Total sector" ~ "Total",
      sub_sector == "Cultivos agrícolas" ~ "Agricultura",
      sub_sector == "Cría de animales" ~ "Pecuario",
      TRUE ~ "Otros"
    )
  ) %>% 
  group_by(anio, cuenta) %>% 
  summarise(
    vab_pb = sum(vab_pb, na.rm = T)
  ) %>% 
  ungroup()


df_output <- df_vab_agro %>% 
  left_join(df_vab_total, join_by(anio)) %>% 
  mutate(
    participacion_vab_pb = vab_pb / vab_pb_total
  ) %>% 
  select(anio, cuenta, participacion_vab_pb)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(participacion_vab_pb = participacion_pbi)



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", 'cuenta'), 
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
  variable_nombre = c("participacion_vab_pb"),
  descripcion = c("Participación en el Valor Agregado Bruto a precios básicos en pesos corrientes")
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
    pk = c("anio",'cuenta'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    cambio_nombre_cols = list('participacion_vab_pb' = 'participacion_pbi'),
    descripcion_columnas = descripcion,
    aclaracion = "Se modifico el cociente, antes se utilizaba como denominador el PIB en pesos corrientes, ahora el denominador es la misma variable que el numerador VAB a precios básicos en pesos corrientes"
    unidades = list("participacion_vab_pb" = "unidades")
  )
