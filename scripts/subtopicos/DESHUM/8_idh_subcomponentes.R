#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



subtopico <- "DESHUM"
output_name <- "idh_subcomponentes"
analista = "Pablo Sonzogni"
fuente1 <- "R216C87" #mys
fuente2 <- "R283C152" #le
fuente3 <- "R284C153" #eys
fuente4 <- "R285C154" #gnipc

all <- c(fuente1, fuente2, fuente3, fuente4)

# Función para leer un archivo Parquet dado un código
leer_parquet <- function(codigo) {
  path <- argendataR::get_clean_path(codigo)
  arrow::read_parquet(path)
}

# Leer todos los data.frames y guardarlos en una lista
lista_dfs <- purrr::map(all, leer_parquet)

# Realizar el join progresivo por 'iso3' y 'anio'
df_all <- purrr::reduce(lista_dfs, ~ full_join(.x, .y, by = c("iso3", "anio"))) 


df_calculos <- df_all %>% 
  mutate(
    le_index = case_when(
      expectativa_vida_al_nacer > 85 ~ 1,
      expectativa_vida_al_nacer < 20 ~ 0,
      TRUE ~ (expectativa_vida_al_nacer - 20) / (85 - 20)
    ),
    eys_index = case_when(
      expectativa_educ > 18 ~ 1,
      expectativa_educ < 0 ~ 0,
      TRUE ~ expectativa_educ / 18
    ),
    mys_index = case_when(
      mean_years_schoolling > 15 ~ 1,
      mean_years_schoolling < 0 ~ 0,
      TRUE ~ mean_years_schoolling / 15
    ),
    ed_index = (eys_index + mys_index) / 2,
    
    gni_index = case_when(
      inb_pc > 75000 ~ 1,
      inb_pc < 100 ~ 0,
      TRUE ~ (log(inb_pc) - log(100))/(log(75000) - log(100))
    ),
    idh_index = (ed_index * gni_index * le_index)^(1/3)
  )

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

nombres_idh <- c(
  'le_index' = "IDH esperanza de vida",
  'eys_index' = "IDH años esperados de escolarización",
  'mys_index' = "IDH años de escolarización 25+",
  'gni_index' = "IDH ingreso per cápita",
  'ed_index' = "IDH escolarización agregado",
  'idh_index' = "IDH agregado"
  
)

codigo_a_string <- function(cod){return(nombres_idh[cod])}

df_output <- df_calculos %>% 
  select(matches("anio|iso3|.*_index")) %>% 
  mutate(
    iso3 = ifelse(grepl(".*WORLD", iso3),"WLD", iso3)
  ) %>% 
  left_join(geonomenclador , join_by(iso3)) %>% 
  mutate(
    pais_nombre = case_when(
      iso3 %in% codigos_hdr ~ poner_nombre[iso3],
      TRUE ~ pais_nombre
    )
  ) %>% 
  pivot_longer(matches(".*_index"), 
               names_to = "idh_tipo",
               names_transform = codigo_a_string, 
               values_to = "valor") 


df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  %>% 
  mutate(
    iso3 = ifelse(grepl(".*WORLD", iso3),"WLD", iso3)
  ) %>% 
  rename(pais_nombre = country)


comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "anio", "idh_tipo"), 
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
  variable_nombre = c("pais_nombre"),
  descripcion = c("Nombre de país de referencia")
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
    pk = c("anio","iso3", "idh_tipo"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'iso3',
    descripcion_columnas = descripcion,
    unidades = list("valor" = "indice")
  )

