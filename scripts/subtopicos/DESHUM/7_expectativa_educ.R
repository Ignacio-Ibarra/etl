#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



subtopico <- "DESHUM"
output_name <- "expectativa_educ"
analista = "Pablo Sonzogni"
fuente1 <- "R284C153"

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar, continente_fundar, nivel_agregacion)


# Cargo data desde server
df_hdr <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

codigos_hdr <- c(
  "ZZA.VHHD",
  "ZZB.HHD",
  "ZZC.MHD",
  "ZZD.LHD",
  "ZZE.AS",
  "ZZF.EAP",
  "ZZG.ECA",
  "ZZH.LAC",
  "ZZI.SA",
  "ZZJ.SSA"
)

# Vector con las descripciones
descripciones <- c(
  "Desarrollo humano muy alto",
  "Desarrollo humano alto",
  "Desarrollo humano medio",
  "Desarrollo humano bajo",
  "Estados Árabes",
  "Este de Asia y Pacífico",
  "Europa y Asia Central",
  "América Latina y el Caribe",
  "Asia meridional",
  "África Subsahariana"
)

poner_nombre <- setNames(descripciones, codigos_hdr)



df_output <- df_hdr %>% 
  mutate(
    iso3 = ifelse(grepl(".*WORLD", iso3),"WLD", iso3)
  ) %>% 
  left_join(geonomenclador , join_by(iso3)) %>% 
  mutate(
    pais_nombre = case_when(
      iso3 %in% codigos_hdr ~ poner_nombre[iso3],
      TRUE ~ pais_nombre
    ),
    nivel_agregacion = case_when(
      iso3 %in% codigos_hdr ~ "agregacion",
      TRUE ~ nivel_agregacion
    )
  )


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  %>% 
  mutate(
    iso3 = ifelse(grepl(".*WORLD", iso3),"WLD", iso3),
    nivel_agregacion = ifelse(is.na(es_agregacion), "agregacion", "pais")
  ) %>% 
  select(-es_agregacion) 

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "anio"), 
  drop_joined_df = F
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
  variable_nombre = c("nivel_agregacion"),
  descripcion = c("Variable que identifica el nivel de agregación de la entidad geográfica ('pais' o 'agregacion')")
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
    pk = c("anio","iso3"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'iso3',
    descripcion_columnas = descripcion,
    unidades = list("expectativa_educ" = "unidades")
  )

