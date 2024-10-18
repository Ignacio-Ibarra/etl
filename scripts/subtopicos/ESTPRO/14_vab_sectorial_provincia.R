################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "vab_sectorial_provincia"
analista = "Gisella Pascuariello"

fuente1 <- "R221C92"
fuente2 <- "R236C0"


get_clean_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}clean/")
  df_fuentes_clean <- fuentes_clean() 
  path_clean <- df_fuentes_clean[df_fuentes_clean$codigo == codigo,c("path_clean")]
  return(paste0(prefix, path_clean))
}


get_raw_path <- function(codigo){
  prefix <- glue::glue("{Sys.getenv('RUTA_FUENTES')}raw/")
  df_fuentes_raw <- fuentes_raw() 
  path_raw <- df_fuentes_raw[df_fuentes_raw$codigo == codigo,c("path_raw")]
  return(paste0(prefix, path_raw))
}


df_cepal <- arrow::read_parquet(get_clean_path(fuente1)) 

dicc_sectores <- readxl::read_excel(get_raw_path(fuente2))

df_output <- df_cepal %>% 
  dplyr::filter(!(provincia == "No distribuido")) %>% 
  mutate(
    gran_region = case_when(
      provincia_id %in% c(2, 6, 14, 30, 42, 50, 70, 74, 82) ~ "Centro",
      provincia_id %in% c(10, 18, 22, 34, 38, 46, 54, 66, 86, 90) ~ "Norte",
      provincia_id %in% c(26, 58, 62, 78, 94) ~ "Sur",
      TRUE ~ NA_character_
    )
  ) %>% 
  left_join(dicc_sectores %>% select(letra, dos_digitos_desc, letra_desc_abrev), join_by(sector_de_actividad_economica == dos_digitos_desc)) %>%
  drop_na(letra) %>% 
  group_by(anio, gran_region, provincia_id, provincia, letra, letra_desc_abrev) %>% 
  summarise(
    vab_pb = sum(vab_pb, na.rm = T)
  ) %>% 
  ungroup() %>% 
  mutate(tipo_sector = ifelse(letra %in% c('A', 'B', 'C', 'D', 'E', 'F'),'Bienes', 'Servicios')) %>% 
  group_by(anio, provincia) %>% 
  mutate(
    share_vab_sectorial = vab_pb / sum(vab_pb, na.rm = T)
  ) %>% 
  ungroup() %>% 
  select(-vab_pb)


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  rename(gran_region = gran_region_desc) %>% 
  select(-gran_region_id)

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","letra", "provincia_id"), # variables pk del dataset para hacer el join entre bases
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
  dplyr::filter(grepl(paste0(output_name,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



gran_region_desc <- df_cepal %>% 
  dplyr::filter(!(provincia == "No distribuido")) %>% 
  mutate(
    gran_region = case_when(
      provincia_id %in% c(2, 6, 14, 30, 42, 50, 70, 74, 82) ~ "Centro",
      provincia_id %in% c(10, 18, 22, 34, 38, 46, 54, 66, 86, 90) ~ "Norte",
      provincia_id %in% c(26, 58, 62, 78, 94) ~ "Sur",
      TRUE ~ NA_character_
    )
  ) %>% 
  distinct(provincia, gran_region) %>% 
  group_by(gran_region) %>% 
  summarise(concatenado = stringr::str_c(provincia, collapse=", ")) %>% 
  ungroup() %>% 
  mutate(concatenado2 = paste0(gran_region,": ", concatenado)) %>%
  pull(concatenado2) %>%
  paste0(., collapse = "; ") %>% 
  paste0("Región de referencia: 'Centro', 'Norte' y 'Sur'. Las provincias que las componen son: ", .)



etiquetas_nuevas <- data.frame(
  variable_nombre = c("gran_region"),
  descripcion = gran_region_desc
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
    pk = c("anio", "letra", "provincia_id"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('gran_region' = 'gran_region_desc'),
    unidades = list("share_vab_sectorial" = "unidades"),
    aclaracion = "En la descripción de las columnas se detalla cuál es la composicón de provincias de cada `gran_region`"
  )
