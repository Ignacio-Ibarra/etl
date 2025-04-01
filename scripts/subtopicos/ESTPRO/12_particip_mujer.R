################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "ESTPRO"
output_name <- "particip_mujer"
analista = "Gisella Pascuariello"

fuente1 <- "R235C105" 

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


df_bel_puestos_priv <- arrow::read_parquet(get_clean_path(fuente1))


anios_completos <- df_bel_puestos_priv %>% distinct(periodo_trimestre_ano) %>% 
  mutate(
    anio = as.integer(str_extract(periodo_trimestre_ano, ".*(\\d{4})", group = 1))
  ) %>% 
  group_by(anio) %>%
  mutate(selection = n() == 4) %>% 
  dplyr::filter(selection) %>% 
  distinct(anio) %>% pull()
  


df_output <- df_bel_puestos_priv %>% 
  mutate(
    anio = as.integer(str_extract(periodo_trimestre_ano, ".*(\\d{4})", group = 1))
  ) %>% 
  group_by(anio, letra, letra_desc_abrev, sexo) %>% 
  summarise(puestos = mean(puestos, na.rm = T)) %>% 
  ungroup() %>% 
  group_by(anio, letra, letra_desc_abrev) %>% 
  mutate(porc_mujeres = puestos / sum(puestos, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(sexo == "Mujer") %>% 
  select(-sexo, -puestos) %>% 
  dplyr::filter(anio %in% anios_completos)
  


# Modifico para que coincida con el nuevo formato
df_anterior <- argendataR::descargar_output(nombre =output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  mutate(anio = as.integer(anio))

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  pk = c("anio","letra"), # variables pk del dataset para hacer el join entre bases
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


descripcion <- armador_descripcion(metadatos = metadatos,output_cols = output_cols)


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
    pk = c("anio", "letra"),
    es_serie_tiempo = T,
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("porc_mujeres" = "unidades")
  )

