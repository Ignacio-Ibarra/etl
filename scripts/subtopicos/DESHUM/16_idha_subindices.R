#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection



subtopico <- "DESHUM"
output_name <- "idha_subindices"
analista = "Pablo Zongzoni"
fuente1 <- "R292C161" # Prados de la Escosura (2021) - Países
fuente2 <- "R293C162"


# Realizar el join progresivo por 'iso3' y 'anio'
df_paises <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

df_regiones <- arrow::read_parquet(argendataR::get_clean_path(fuente2))

variables_interes <- c(
  "Kakwani Index of Life Expectancy" = "Esperanza de vida", 
  "Kakwani Index of Schooling" = "Años de escolarización", 
  "Liberal Democracy Index" = "Democracia liberal",
  "UNDP Adjusted Per Capita Income" = "PIB per cápita",
  "Augmented Human Development Index" = "Total"
)

df_output_paises <- df_paises %>% 
  dplyr::filter(variable_name %in% names(variables_interes)) %>% 
  mutate(
    idha_subdimension = variables_interes[variable_name],
    nivel_agregacion = "pais"
  ) %>% 
  select(-variable_id, -variable_name, -region) %>% 
  rename(area_desc = pais_nombre)


df_output_regiones <- df_regiones %>% 
  dplyr::filter(variable_name %in% names(variables_interes)) %>% 
  mutate(
    idha_subdimension = variables_interes[variable_name],
    nivel_agregacion = 'region'
  ) %>% 
  select(-variable_id, -variable_name) %>% 
  rename(area_desc = region)

df_output <- bind_rows(df_output_paises, df_output_regiones)

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega")  %>% 
  rename(area_desc = iso3_desc_fundar) %>% 
  mutate(
    iso3 = case_when( 
      iso3 == "AHDI.WLD" ~ "WLD", 
      iso3 == "CSK" ~ 'CZE', 
      TRUE ~ iso3)
  )



comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("iso3", "anio", "idha_subdimension"), 
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
  variable_nombre = c("area_desc",
                      "nivel_agregacion"),
  descripcion = c("Nombre de país o región de referencia",
                  "Indica si el registro refiere a un 'pais' o 'agregacion'")
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
    pk = c("iso3", "idha_subdimension", "anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'iso3',
    descripcion_columnas = descripcion,
    unidades = list("valor" = "unidades")
    )