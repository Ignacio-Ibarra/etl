################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "PBG_participacion_agro_provincia_evolucion"
analista = "Daniel Schteingart y Juan Gabriel Juara"
fuente1 <- "R221C92" # Desagregacion provincial CEPAL-MECON

df_cepal <- arrow::read_parquet(argendataR::get_clean_path(fuente1))


df_output <- df_cepal %>% 
  dplyr::filter(sector_de_actividad_economica %in% c("Total sectores"
                                                     , "Agricultura, ganaderia, caza y servicios conexos"
                                                     #, "Silvicultura, extracción de madera y servicios conexos"
                                                     #, "Pesca"
                                                     )) %>% 
  mutate(sector = ifelse(sector_de_actividad_economica == "Total sectores", "todo", "agro")) %>% 
  group_by(anio, sector, provincia_id, provincia) %>% 
  summarise(
    vab_pb = sum(vab_pb, na.rm = T)
  ) %>% 
  ungroup() %>% 
  pivot_wider(
    id_cols = all_of(c('anio', 'provincia_id', 'provincia')),
    names_from = "sector",
    values_from = "vab_pb"
  ) %>% 
  mutate(participacion_agro_vab_pb = agro / todo) %>% 
  select(anio, provincia_id, provincia, participacion_agro_vab_pb) %>% 
  drop_na(provincia_id)
  


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(participacion_agro_vab_pb = valor,
         provincia_id = cod_pcia,
         provincia = nom_pcia)


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio",'provincia_id'), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("provincia",
                      "provincia_id",
                      "participacion_agro_vab_pb"
                      ),
  descripcion = c(
    "Nombre de la provincia",
    "Codigo de la provincia",
    "Participación del sector 'Agricultura, ganaderia, caza y servicios conexos' en el VAB a precios básicos en pesos constantes de 2004")
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
    pk = c("anio",'provincia_id'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    columna_geo_referencia = 'provincia_id',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list(
      'participacion_agro_vab_pb' = 'valor',
      'provincia_id' = 'cod_pcia',
      'provincia' = 'nom_pcia'
    ),
    unidades = list("participacion_agro_vab_pb" = "unidades")
  )

