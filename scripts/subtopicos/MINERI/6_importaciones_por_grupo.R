################################################################################
##                              Dataset: nombre                               ##
################################################################################

# Este script es una copia del script ~/etl/scripts/subtopicos/ACECON/7_pib_comp_va.R


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "importaciones_por_grupo"
analista = "Kevin Corfield"
fuente1 <- "R270C139" # SIACAM


df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) 

metales <- c("Cinc", "Ferroaleaciones", "Aluminio", "Hierro")

df_siacam <- df_siacam %>% 
  mutate(grupo_agrup_nuevo = ifelse(grupo_agrup %in% metales, grupo_agrup, "Otros")) %>% 
  mutate(grupo_agrup_nuevo = ifelse(grupo_agrup_nuevo == "Cinc", "Zinc", grupo_agrup_nuevo))

otros <- df_siacam %>% 
  dplyr::filter(grupo_agrup_nuevo == "Otros") %>% 
  ungroup() %>% 
  group_by(grupo) %>%
  summarise(
    cif = sum(cif, na.rm = T)
    ) %>% 
  ungroup() %>% 
  arrange(-cif) %>% 
  mutate(cumulative_share = cumsum(cif) / sum(cif)) %>% 
  dplyr::filter(cumulative_share<0.8) %>% pull(grupo)


aclaracion <- otros  %>% 
  paste0(., collapse = ", ") %>% 
  paste0("El grupo 'Otros' incluye los siguientes minerales: ",.,", entre los más importantes.")

df_output <- df_siacam %>% 
  group_by(anio = anyo, grupo = grupo_agrup_nuevo) %>%  
  summarise(impo_grupo = sum(cif, na.rm = T)) %>% 
  ungroup() %>% 
  complete(anio, grupo, fill = list(impo_grupo = 0)) %>% 
  arrange(anio, -impo_grupo)



df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  rename(grupo = grupo_nuevo) %>%
  mutate(anio = as.integer(anio)) %>% 
  mutate(grupo = tools::toTitleCase(grupo))



comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "grupo"), # variables pk del dataset para hacer el join entre bases
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
  variable_nombre = c("grupo",
                      "impo_grupo"),
  descripcion = c("Clasificación por grupo de minerales",
                  "Importaciones en dólares estadounidenses, en precios corrientes")
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
    pk = c("anio","grupo"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    cambio_nombre_cols = list("grupo" = "grupo_nuevo"),
    unidades = list("impo_grupo" = "unidades"),
    aclaracion = aclaracion
  )



