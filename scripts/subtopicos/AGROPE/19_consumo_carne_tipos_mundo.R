################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "consumo_carne_tipos_mundo"
output_name_old <- "consumo_carne_tipos_mundo_2020"
analista = "Franco A. Mendoza y Kevin Corfield"
fuente1 <- "R299C167" # FAO FBS


df_fao_fbs <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 


carnes <- c("Bovine Meat" = "Vacuna",
            "Meat, Aquatic Mammals" = "Pescados y mariscos",
            "Meat, Other" = "Otras carnes",
            "Mutton & Goat Meat" = "Caprina y ovina",
            "Pigmeat" = "Porcina",
            "Poultry Meat" = "Aviar")

df_fao_fbs_filtered <- df_fao_fbs %>% 
  dplyr::filter(item %in% names(carnes), element == "Food supply quantity (kg/capita/yr)")  %>% 
  select(-flags, -notes, -element_code, -element) 


df_output <- df_fao_fbs_filtered %>% 
  mutate(tipo_carne = carnes[item]) %>% 
  select(anio = year, iso3, pais, tipo_carne, valor = value)





df_anterior <- argendataR::descargar_output(nombre = output_name_old,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(
    tipo_carne = case_when(
      tipo_carne == "aviar" ~ "Aviar",
      tipo_carne == "vacuna" ~ "Vacuna",
      tipo_carne == "caprina_ovina" ~ "Caprina y ovina",
      tipo_carne == "porcina" ~ "Porcina",
      tipo_carne == "otras_carnes" ~ "Otras carnes",
      TRUE ~ "Pescados y mariscos",
    )
  ) %>% 
  rename(pais = iso3_desc_fundar) 


comparable_df <- df_output %>% dplyr::filter(anio == 2020) %>% select(-anio)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = comparable_df,
  nombre = output_name,
  pk = c('tipo_carne','iso3'), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("tipo_carne", 
                      "pais"),
  descripcion = c("Tipo de carne: 'Aviar', 'Porcina', 'Vacuna', 'Caprina y ovina' y 'Pescados y mariscos'",
                  "Pais de referencia")
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
    pk = c("tipo_carne",'anio', 'iso3'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_output = list("nombre_nuevo" = output_name, 'nombre_anterior' = output_name_old),
    cambio_nombre_cols = list('pais' = 'iso3_desc_fundar'),
    unidades = list("valor" = "kilogramos por año por persona"),
    aclaraciones = "Se modificó la fuente de información, antes era OWID ahora es FAO. Los datos son distintos a los de OWID. Se cambia el formato a serie de tiempo del dataset"
  )