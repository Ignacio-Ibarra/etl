################################################################################
##                              Dataset: nombre                               ##
################################################################################
#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "participacion_CAA_PIB_arg_evo"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R341C214"
fuente2 <- "R341C215"
fuente3 <- "R38C7"

df_caa_constantes <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet(.) %>% 
  mutate(cadena_merge = toupper(stringi::stri_trans_general(cadena, "Latin-ASCII")))

df_caa_implicitos <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet(.) %>% 
  mutate(cadena_merge = toupper(stringi::stri_trans_general(cadena, "Latin-ASCII"))) %>% 
  select(-cadena)

df_pib_pcorr <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet(.) %>% 
  dplyr::filter(trim == "Total", indicador == "valor_agregado_bruto_a_precios_basicos") %>% 
  mutate(vab_total = valor / 1000000) %>% 
  select(anio, vab_total)
  


df_output <- df_caa_constantes %>% 
  inner_join(df_caa_implicitos, join_by(cadena_merge, anio)) %>% 
  select(-cadena_merge) %>% 
  mutate(vab_pcorr = vab * indice / 100, 
         unidad_medida = "millones de pesos corrientes",
         anio = as.integer(anio)) %>% 
  select(cadena, anio, vab_pcorr, unidad_medida) %>% 
  group_by(anio) %>% 
  summarise(
    vab_pcorr = sum(vab_pcorr, na.rm = T)
  ) %>% 
  inner_join(df_pib_pcorr, join_by(anio)) %>% 
  mutate(
    share_caa = vab_pcorr / vab_total
  ) %>% 
  select(anio, vab_pcorr, share_caa)
  


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(share_caa = valor)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("vab_pcorr",
                      "share_caa"
  ),
  descripcion = c(
    "Valor Agregado Bruto en precios básicos, en millones de pesos corrientes",
    "Participación de las CAA en el Valor Agregado Bruto a precios básicos en pesos corrientes"
  )
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
    pk =  c("anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('share_caa' = 'valor'),
    unidades = list("vab_pcorr" = "millones de pesos corrientes",
                    "share_caa" = "proporción"),
    aclaraciones = "Se tomaron los datos del Cuadro 6. CAA. VALOR AGREGADO BRUTO A PRECIOS CONSTANTES DE 2007 POR AÑO SEGÚN CAA (EN MILES DE MILLONES).
    Se tomaron los datos del Cuadro 7. CAA. PRECIOS IMPLÍCITOS POR AÑO SEGÚN CAA (BASE 2007=100). 
    Con dichas variables se construyó la variable a pesos corrientes, para el total de las CAA. 
    Se tomó el dato de VABpb (no PIBpm, a diferencia del cálculo hecho por los analistas) de INDEC a pesos corrientes.
    Con esas dos variables se calculó la participación del VABpb de todas las CAA en el VABpb de la economía, en pesos corrientes"
  )
