################################################################################
##                              Dataset: nombre                               ##
################################################################################

# Este script es una copia del script ~/etl/scripts/subtopicos/ACECON/7_pib_comp_va.R


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "MINERI"
output_name <- "expo_por_sectores"
analista = "Kevin Corfield"
fuente1 <- "R269C136" # Balanza 1910 en adelante (ICA INDEC)
fuente2 <- "R268C137" # SIACAM


df_ica <- arrow::read_parquet(argendataR::get_clean_path(fuente1)) %>% 
  select(anio, fob_total = exportacion) %>% 
  mutate(fob_total = fob_total * 1000)

df_siacam <- arrow::read_parquet(argendataR::get_clean_path(fuente2)) 

no_otros <- c(
  "Metaliferos", 
  "No metaliferos",
  "Piedras preciosas y/o semipreciosas",
  "Rocas de aplicación"
)

df_output <- df_siacam %>%
  rename(anio = anyo) %>% 
  mutate(sector = ifelse(sector %in% no_otros, sector, "Otros")) %>% 
  group_by(anio, sector) %>% 
  summarise(fob_siacam = round(sum(fob, na.rm=T))) %>% 
  left_join(df_ica, join_by(anio)) %>% 
  mutate(exportaciones_sector_perc = round(100*fob_siacam / fob_total, 3)) %>% 
  select(anio, sector, exportaciones_sector_perc)


recoder <- c(
    "metaliferos" = "Metaliferos", 
    "no_metaliferos" = "No metaliferos",
    "piedras_preciosas_semipreciosas" = "Piedras preciosas y/o semipreciosas",
    "rocas_de_aplicacion" = "Rocas de aplicación",
    "otros" = "Otros"
  )


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") %>% 
  mutate(
    sector = recode(sector, !!!recoder),
    anio  = as.integer(anio))


comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", "sector"), # variables pk del dataset para hacer el join entre bases
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



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
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
    pk = c("anio"),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("exportaciones_sector_perc" = "porcentaje")
    # aclaracion = aclaracion
  )
