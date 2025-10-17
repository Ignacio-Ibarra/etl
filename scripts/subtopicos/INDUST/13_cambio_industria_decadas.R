#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "cambio_industria_decadas.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R221C92' # PBG CEPAL CEP
fuente2 <- 'R461C0' # Libro MinDeP


# carga de fuentes - nico 
df_cepal <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

df_salles <- argendataR::get_raw_path(fuente2) %>% 
  read.csv() %>% 
  janitor::clean_names()  



# armar datos pbg 
seleccionar_industrias_pbg <- df_cepal %>% 
  select(sector_de_actividad_economica) %>% 
  distinct() %>%  
  filter(row_number() > 5 & row_number() < 30) %>% 
  pull(sector_de_actividad_economica)

df_cepal_pbg_industrial <- df_cepal %>% 
   dplyr::filter(sector_de_actividad_economica %in% seleccionar_industrias_pbg, 
                 provincia != 'No distribuido') %>% 
  group_by(anio,provincia,provincia_id) %>% 
  summarize(vab_pb = sum(vab_pb)) %>% 
  ungroup() %>% 
  group_by(anio) %>% 
  mutate(prop = vab_pb / sum(vab_pb)) %>%
  ungroup() %>% 
  select(anio,provincia_id,provincia,prop)


dicc_prpovincias <- df_cepal_pbg_industrial %>% distinct(provincia_id, provincia)

df_pbg_hist <- df_salles %>% 
  pivot_longer(-provincia,
               names_to='anio',
               values_to='prop', 
               names_transform = function(x){as.numeric(str_remove(x,"x"))}) %>% 
  mutate(provincia = case_when(provincia == 'Ciudad de Buenos Aires'~ 'CABA',
                               TRUE ~ provincia)) %>% 
  left_join(dicc_prpovincias, join_by(provincia))


df_output <- bind_rows(
  df_pbg_hist, 
  df_cepal_pbg_industrial
  ) %>% 
  arrange(provincia_id, anio) %>% 
  select(anio, provincia_id, provincia, prop)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico) 

pks_comparacion <- c('anio','provincia_id')

comparacion <- argendataR::comparar_outputs(
  df = df_output,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = pks_comparacion
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
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")


