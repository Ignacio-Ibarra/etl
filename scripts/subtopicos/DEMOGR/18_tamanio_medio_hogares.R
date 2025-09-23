# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "DEMOGR"
output_name <- "tamanio_medio_hogares.csv"
analista <- "Rafael Rofman & Ximena de la Fuente"

fuente1 <- 'R445C0' # INDEC. Censo 2022, Historia (1869-2010).
fuente2 <- "R447C288" # INDEC. Censo 2022 - Viviendas particulares por jurisdiccion
fuente3 <- 'R448C289' # INDEC. Censo 2022 - Población en viviendas particulares por jurisdisccion. 

df_historia <- argendataR::get_raw_path(fuente1) %>% 
  read.csv() 

df_viv2022 <- argendataR::get_clean_path(fuente2) %>% 
  arrow::read_parquet() %>% 
  summarise(
    viviendas = sum(viviendas_particulares),
    anio = 2022
  )

df_pob2022 <- argendataR::get_clean_path(fuente3) %>% 
  arrow::read_parquet() %>% 
  dplyr::filter(sexo == "Ambos") %>% 
  summarise(
    habitantes = sum(poblacion_viviendas_particulares),
    anio = 2022
  )


df_tamanio_2010 <- df_historia %>% 
  drop_na(viviendas) %>% 
  mutate(tamanio_medio_hogares = habitantes / viviendas) %>% 
  select(anio, tamanio_medio_hogares)




df_tamanio_2022 <-  df_viv2022 %>% 
  left_join(df_pob2022, join_by(anio)) %>% 
  mutate(tamanio_medio_hogares = habitantes / viviendas) %>% 
  select(anio, tamanio_medio_hogares)


df_output <- df_tamanio_2010 %>% 
  bind_rows(df_tamanio_2022) %>% 
  dplyr::filter(anio>=1947) # Los datos anteriores no son comparables
  
df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico)

pks <- c('anio')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks
)


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
  dplyr::filter(grepl(paste0("^", output_name), nombre_archivo)) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output



descripcion <- armador_descripcion(metadatos = metadatos,
                                   # etiquetas_nuevas = etiquetas_nuevas,
                                   output_cols = output_cols)



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    subtopico = subtopico,
    control = comparacion, 
    fuentes = argendataR::colectar_fuentes(),
    analista = analista,
    pk = pks,
    descripcion_columnas = descripcion, 
    unidad = list("tamanio_medio_hogares" = "unidades"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")

