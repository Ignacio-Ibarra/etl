#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "comex_arg_agroindustria_MOA_PP_evo"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R306C174" # Subsecretaría de Programación Económica - Valores y Metadatos.  


df_prog <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 



df_expo_moa <- df_prog %>% 
  dplyr::filter(distribucion_id == 75.1) %>% 
  dplyr::filter(serie_titulo == "ica_total_moa") %>% 
  select(indice_tiempo, moa = valor)


df_expo_pp <- df_prog %>% 
  dplyr::filter(distribucion_id == 75.1) %>% 
  dplyr::filter(serie_titulo == "ica_exportaciones_total_productos_primarios") %>%
  select(indice_tiempo, pp = valor)


df_expo_pp_no_agro <- df_prog %>% 
  dplyr::filter(distribucion_id == 162.1) %>% 
  dplyr::filter(serie_titulo %in% c("x_minerales_metaliferos_escorias_cenizas", # Capitulo 26
                                    "x_sal_azufre_tierras_piedras_yesos_cales_cementos")  # Capitulo 25
  ) %>% 
  group_by(indice_tiempo) %>% 
  summarise( pp_no_agro = sum(valor, na.rm = T)) %>% 
  ungroup()

df_expo_total <- df_prog %>% 
  dplyr::filter(distribucion_id == 75.1) %>% 
  dplyr::filter(serie_titulo == "ica_exportaciones_total_general") %>% 
  select(indice_tiempo, total = valor)


df_output <- purrr::reduce(list(df_expo_moa, df_expo_pp, df_expo_pp_no_agro, df_expo_total), ~ left_join(.x, .y, by = "indice_tiempo")) %>% 
  mutate(pp_sin_mineria = pp - pp_no_agro,
         participacion_expo_totales = (moa + pp_sin_mineria) / total,
         anio = year(as.Date(indice_tiempo))
         ) %>% 
  select(anio, pp_sin_mineria, moa, participacion_expo_totales)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(pp_sin_mineria = productos_primarios) %>% 
  mutate(anio  = as.integer(anio))




comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c('anio'),
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
  variable_nombre = c("moa", 
                      "pp_sin_mineria"),
  descripcion = c("Valor de exportaciones del rubro manufacturas de origen agropecuario (MOA),  en millones de dólares",
                  "Valor de exportaciones del rubro productos primarios (PP), exluyendo capítulos 25 y 26 del NCM, en millones de dólares")
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


aclaracion = c(
  "Se modificó la fuente de información debido a la facilidad de procesamiento, antes eran varios archivos pdf",
  "Se incorporaron todos los años disponibles en la fuente de datos",
  "Para el rubro Productos Primarios se sacaron los productos de dedicados a los productos primarios mineros (Capitulo 25 y 26 del NCM)")

df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = analista,
    pk = c('anio'),
    control = comparacion, 
    cambio_nombre_cols = list("pp_sin_mineria" = "productos_primarios"),
    descripcion_columnas = descripcion,
    unidades = list("moa" = "millones de dólares",
                    "pp_sin_mineria" = "millones de dólares",
                    "participacion_expo_totales" = "unidades"),
    aclaraciones = paste0(aclaracion, collapse = ". ")
  )
