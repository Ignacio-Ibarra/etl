################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "particip_mujer_varon"
analista = "Gisella Pascuariello"

fuente1 <- "R228C98" # ar
fuente2 <- "R228C99" # anr
fuente3 <- "R228C100" # na

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


df_ar <- arrow::read_parquet(get_clean_path(fuente1)) %>% 
  dplyr::filter(edad_sexo %in% c("Varones", "Mujeres"))

df_anr <- arrow::read_parquet(get_clean_path(fuente2)) %>% 
  dplyr::filter(edad_sexo %in% c("Varones", "Mujeres"))

df_na <- arrow::read_parquet(get_clean_path(fuente3)) %>% 
  dplyr::filter(edad_sexo %in% c("Varones", "Mujeres"))


base <- df_ar %>% 
  left_join(df_anr) %>% 
  left_join(df_na) %>% 
  pivot_longer(!all_of(c("anio","letra","letra_desc","edad_sexo")), names_to = "variable", values_to = "value") %>% 
  group_by(letra, letra_desc, anio, sexo = edad_sexo) %>% 
  summarise(puestos = sum(value, na.rm = T))

df_letra <- base %>% 
  group_by(letra, letra_desc, anio) %>% 
  mutate(participacion = puestos / sum(puestos, na.rm = T)) %>% 
  ungroup() %>% 
  select(-puestos)

df_total <- base %>% 
  group_by(anio, sexo) %>% 
  summarise(puestos = sum(puestos, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(participacion = puestos / sum(puestos, na.rm = T)) %>% 
  ungroup() %>% 
  select(-puestos) %>% 
  mutate(letra = "Z",
         letra_desc = "Total")


df_output <- df_letra %>% 
  bind_rows(df_total) %>% 
  rename(letra_desc_abrev = letra_desc)


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  pivot_longer(!all_of(c("anio","letra","letra_desc_abrev")), 
               names_to = "sexo", 
               values_to = "participacion",
               names_prefix = "porc_",
               names_transform = stringr::str_to_title ) %>% 
  mutate(anio = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","letra", "sexo"), # variables pk del dataset para hacer el join entre bases
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


etiquetas_nuevas <- data.frame(
  variable_nombre = c("sexo",
                      "participacion"),
  descripcion = c("Referencia al sexo: 'Muejeres' y 'Varones'",
                  "Proporción de trabajadores según sexo")
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
    pk = c("anio", "letra", "sexo"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("participacion" = "unidades")
  )
