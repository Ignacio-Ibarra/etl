################################################################################
##                              Dataset: nombre                               ##
################################################################################

#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "peso_va_min_por_provincia"
analista = "Kevin Corfield"
fuente1 <- "R221C92" 


df_cepal <- arrow::read_parquet(argendataR::get_clean_path(fuente1))

df_output <- df_cepal %>% 
  dplyr::filter(sector_de_actividad_economica != "Total sectores") %>% # El input viene con Total Sectores
  mutate(industria = ifelse(sector_de_actividad_economica == "Extracción de minerales metalíferos. Explotación de  minas y canteras n.c.p.", "mineria", "resto")) %>% 
  dplyr::filter(provincia != "No distribuido") %>% 
  group_by(anio, provincia_id, provincia, industria) %>% 
  summarise(vab_pb = round(sum(vab_pb, na.rm = T),3)) %>% 
  ungroup() %>% 
  group_by(anio, provincia_id, provincia) %>% 
  mutate(participacion = 100*vab_pb / sum(vab_pb, na.rm = T)) %>% 
  ungroup() %>% 
  dplyr::filter(industria == "mineria") %>% 
  select(-industria, -vab_pb)
  

# Hago esta transformacion para que el dataset quede anual
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(participacion = vab_min_vab_total_prov) %>% 
  mutate(provincia = str_replace_all(provincia, "_", " "))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "provincia"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("participacion",
                      "provincia_id"),
  descripcion = c("Participación del sector 'Extracción de minerales metalíferos. Explotación de  minas y canteras n.c.p.' en el Valor Agregado Bruto a precios básicos, en pesos constantes de 2004",
                  "Codigo de provincia, según INDEC")
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
    pk = c("anio", "provincia_id"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_geo_referencia = 'provincia_id',
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list('participacion' = 'vab_min_vab_total_prov'),
    unidades = list("participacion" = "porcentaje"),
    aclaracion = "Se cambiaron levemente los nombres de las provincias, porque no llevaban tildes. Se cambio el nombre de la variable indicador 'participacion' = 'vab_min_vab_total_prov' y se adiciona la columna provincia_id."
  )

