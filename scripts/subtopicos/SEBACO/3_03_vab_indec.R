#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

subtopico <- "SEBACO"
output_name <- "03_vab_indec"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R308C177' # INDEC - COU 2004
fuente2 <- 'R309C178' # INDEC - COU 2018
fuente3 <- 'R310C179' # INDEC - COU 2019
fuente4 <- 'R311C180' # INDEC - COU 2020
fuente5 <- 'R312C181' # INDEC - COU 2021



# Fuentes con años correspondientes
sources <- list(
  'R308C177' = 2004,
  'R309C178' = 2018,
  'R310C179' = 2019,
  'R311C180' = 2020,
  'R312C181' = 2021
)

# Función para leer, castear columnas como character y agregar el año
read_with_year <- function(file, year) {
  df <- arrow::read_parquet(file) %>%
    mutate(anio = as.integer(year))  # Agregar columna con el año
  return(df)
}

# Obtener paths de los archivos
all_paths <- purrr::map_chr(names(sources), ~ argendataR::get_clean_path(.x))

# Leer y combinar datasets agregando el año correspondiente
df_cou <- map2_dfr(all_paths, sources, ~ read_with_year(.x, .y))

df_72_vab <- df_cou %>%
  dplyr::filter(codigo_sector == "72", producto == "VAB") %>%
  select(anio, vab_72 = valor)

df_ui_vab <- df_cou %>%
  mutate(seleccion_codigos = grepl("^[0-9]+.*",codigo_sector) ) %>% 
  dplyr::filter(seleccion_codigos, producto == "VAB") %>%
  group_by(anio) %>% 
  summarise(
    ui_total_vab = sum(valor, na.rm = T)
  ) %>% 
  ungroup()


df_output <- df_72_vab %>% 
  left_join(df_ui_vab, join_by(anio)) %>% 
  mutate(share_vab = vab_72/ui_total_vab) %>% 
  select(anio, share_vab)




df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(share_vab = vab) %>% 
  mutate(anio = as.integer(anio))


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
  variable_nombre = c("share_vab"),
  descripcion = c("Proporción del sector 'Software y Servicios informáticos (72 - CIIU Rev 3 )' en el VAB a precios básicos")
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


df_output %>%
  argendataR::write_output(
    output_name = output_name,
    fuentes = colectar_fuentes(),
    subtopico = subtopico,
    analista = analista,
    pk = c('anio'),
    es_serie_tiempo = T,
    control = comparacion, 
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('share_vab' = 'vab'),
    unidades = list("share_vab" = "proporción")
  )
