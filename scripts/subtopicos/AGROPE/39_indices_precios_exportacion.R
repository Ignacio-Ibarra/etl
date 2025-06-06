#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "indices_precios_exportacion"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R307C176" # INDEC - Indices de precios de exportación por rubro seleccionado

df_precios_rubros <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

rubros <- c(
   "cereales" = "Cereales"
  ,"semillas_frutos_oleaginosos" = "Semillas y frutos oleaginosos"
  ,"grasas_aceites" = "Grasas y aceites"
  ,"residuos_alimenticios" = "Residuos y desperdicios de la industria alimenticia (1)"
  ,"carnes_preparados" = "Carnes y sus preparados"
  )

df_output <- df_precios_rubros %>% 
  dplyr::filter(rubro_seleccionado %in% unname(rubros))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(rubro_seleccionado = rubros[producto_exportacion],
         anio = as.integer(anio)) %>% 
  select(anio, rubro_seleccionado, indice_precio_exportacion = valor)




comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio','rubro_seleccionado'),
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
  variable_nombre = c("rubro_seleccionado", 
                      "indice_precio_exportacion"),
  descripcion = c("Rubro seleccionado",
                  "Índice de precio de exportación, base 2004=100")
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


aclaracion = "(1) Residuos y desperdicios de la industria alimenticia: comprende pellets y harinas de girasol y soja, entre otros productos incluidos en el Capítulo 23 de la Nomenclatura del Sistema Armonizado (NSA)."

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c('anio'),
    control = comparacion, 
    cambio_nombre_cols = list("rubro_seleccionado" = "producto_exportacion",
                              "indice_precio_exportacion" = "valor"),
    descripcion_columnas = descripcion,
    unidades = list("indice_precio_exportacion" = "indice"),
    aclaraciones = aclaracion
  )
