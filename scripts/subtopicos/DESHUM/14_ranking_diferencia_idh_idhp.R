#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



subtopico <- "DESHUM"
output_name_anterior <- "ranking_diferencia_idh_idhp_2022_flourish"
output_name <- "ranking_diferencia_idh_idhp"
analista = "Pablo Zongzoni"
fuente1 <- "R282C151" # hdi
fuente2 <- "R291C160" # phdi

all <- c(fuente1, fuente2)

# Función para leer un archivo Parquet dado un código
leer_parquet <- function(codigo) {
  path <- argendataR::get_clean_path(codigo)
  arrow::read_parquet(path)
}

# Leer todos los data.frames y guardarlos en una lista
lista_dfs <- purrr::map(all, leer_parquet)

# Realizar el join progresivo por 'iso3' y 'anio'
df_all <- purrr::reduce(lista_dfs, ~ inner_join(.x, .y, by = c("iso3", "anio"))) 


geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3 = codigo_fundar, pais_nombre = desc_fundar)

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
  "ZZJ.SSA",
  "ZZK.WORLD"
)




df_output <- df_all %>% 
  dplyr::filter(!(iso3 %in% codigos_hdr)) %>% # se quitan las filas de las regiones geográficas
  dplyr::filter(anio == max(anio)) %>% # filtarmos solo el ultimo año
  drop_na(phdi_value, hdi_value) %>% # eliminamos los registros que no tienen disponible el dato de phdi o hdi_value
  select(-anio) %>% 
  mutate(
    hdi_ranking = rank(-hdi_value, ties.method = "first"),
    phdi_ranking = rank(-phdi_value, ties.method = "first")
  ) %>%
  pivot_longer(
    cols = matches("hdi.*|phdi.*"), # columnas a pivotear
    names_to = c("tipo_idh", "metrica"), # nombres de las nuevas columnas
    names_sep = "_", # separador en los nombres originales
    values_to = "valor"
  ) %>% 
  left_join(geonomenclador , join_by(iso3)) %>% 
  mutate(
    pais_nombre = case_when(
      iso3 %in% codigos_hdr ~ poner_nombre[iso3],
      TRUE ~ pais_nombre
    ),
    metrica = ifelse(metrica == "value", "Índice", "Ranking"),
    tipo_idh = ifelse(tipo_idh == "hdi", "IDH", "IDH-P")
  )


df_anterior <- argendataR::descargar_output(nombre = output_name_anterior, subtopico = subtopico, entrega_subtopico = "primera_entrega")  %>% 
  mutate(
    iso3 = ifelse(grepl(".*WORLD", iso3),"WLD", iso3)
  ) %>% 
  rename(pais_nombre = country,
         valor = ranking_2022) %>%
  mutate(
    metrica = "Ranking"
  )
  


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "tipo_idh", "metrica"), 
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
  dplyr::filter(grepl(paste0("^", output_name_anterior,".csv"), dataset_archivo)) %>% 
  distinct(variable_nombre, descripcion) 


# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("pais_nombre",
                      "metrica",
                      "valor"),
  descripcion = c("Nombre de país de referencia",
                  "Refiere a si el registro corresponde a 'Ranking' o 'Indice'",
                  "Valor observado")
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
    pk = c("iso3", "tipo_idh", "metrica"),
    es_serie_tiempo = T,
    control = comparacion, 
    cambio_nombre_output = list('nombre_nuevo' = output_name, 'nombre_anterior' = output_name_anterior),
    columna_geo_referencia = 'iso3',
    descripcion_columnas = descripcion,
    unidades = list("valor" = "unidades"),
    aclaraciones = "El dataset contiene más países que el dataset anterior, el cual era sólo una selección de países. El ranking calculado, si bien se encuentra emparenteado con el calculado por el analista, mantiene algunas diferencias leves. Esto se debe a que la métrica es muy sensible a la cantidad de países con el cual se realiza el ranking."
  )
