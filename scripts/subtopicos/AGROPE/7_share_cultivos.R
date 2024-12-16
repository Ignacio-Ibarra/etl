################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "share_cultivos"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R296C0" # Estimaciones Agrícolas - MAGyP

df_magyp <- read.csv(argendataR::get_raw_path(fuente1), 
                     na.strings = c("NA", "#N/A", "SD", " "),
                     fileEncoding = "ISO-8859-1")

sacar <- c('Poroto alubia', 'Poroto negro', 'Poroto otros', 'Soja 1ra', 'Soja 2da', 'Trigo candeal')

df_output <- df_magyp %>% 
  dplyr::filter(!(cultivo %in% sacar)) %>% 
  group_by(campania = ciclo, cultivo) %>% 
  summarise(q_total = as.numeric(sum(produccion, na.rm = T))) %>% 
  ungroup() %>% 
  group_by(campania) %>% 
  mutate(
    share = 100* q_total / sum(q_total, na.rm = T)
  ) %>% 
  ungroup()



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 

# Armo un data.frame comparable poniendo la misma campaña. 
df_comparable <- df_magyp %>% 
  dplyr::filter(!(cultivo %in% sacar)) %>% 
  dplyr::filter(ciclo == "2021/2022") %>% 
  group_by(campania = ciclo, cultivo)  %>% 
  summarise(q_total = as.numeric(sum(produccion, na.rm = T))) %>% 
  ungroup() %>% 
  group_by(campania) %>% 
  mutate(
    share = 100* q_total / sum(q_total, na.rm = T)
  ) %>% 
  ungroup()



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_comparable,
  nombre = output_name,
  pk = c('cultivo'), # variables pk del dataset para hacer el join entre bases
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


descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
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
    pk = c("campania",'cultivo'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'campania',
    descripcion_columnas = descripcion,
    unidades = list("q_total" = "toneladas",
                    "share" = "porcentaje"),
    aclaraciones = "Se agregan todas las campanias que tiene el dataset"
  )

