################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "produccion_global_carne_aviar_ultimo_anio_disponible"
output_name_old <- "produccion_global_carne_aviar_2021"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R297C165" # FAO QCL

df_fao <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

# 
# carnes <- c("Meat of cattle with the bone, fresh or chilled" = "Carne bovina", 
#             "Meat of chickens, fresh or chilled" = "Carne aviar", 
#             "Meat of pig with the bone, fresh or chilled" = "Carne de cerdo")

df_carnes <- df_fao %>% 
  dplyr::filter(
    element == "Production", 
    grepl(".*\\bmeat\\b.*", item, ignore.case = TRUE), 
    item != "Meat, Total") %>% 
  select(anio = year, iso3, pais, item, valor = value) 

carne_aviar <- c("Meat of chickens, fresh or chilled")

df_output <- df_carnes %>%
  dplyr::filter(item %in% carne_aviar, anio == max(anio)) %>% 
  group_by(iso3, pais) %>% #supongo que dentro de lo que es carne vacuna, no va a diferir el ultimo año disponible
  summarise(
    valor = sum(valor, na.rm = T)
  ) %>% 
  ungroup()



df_anterior <- argendataR::descargar_output(nombre = output_name_old,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(pais = iso3_desc_fundar)


df_comparable <- df_carnes %>%
  dplyr::filter(item %in% carne_aviar, anio == (max(anio) - 1)) %>% 
  group_by(iso3, pais) %>% #supongo que dentro de lo que es carne vacuna, no va a diferir el ultimo año disponible
  summarise(
    valor = sum(valor, na.rm = T)
  ) %>% 
  ungroup()

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_comparable,
  nombre = output_name,
  pk = c('iso3'), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("pais"),
  descripcion = c("Pais de referencia")
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
    pk = c("iso3"),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    cambio_nombre_output = list("nombre_nuevo" = output_name, 'nombre_anterior' = output_name_old),
    cambio_nombre_cols = list('pais' = 'iso3_desc_fundar'),
    unidades = list("valor" = "toneladas"),
    aclaraciones = "Se actualizó el año"
  )
