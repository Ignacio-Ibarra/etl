#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

# Metadatos 
subtopico <- "INDUST"
output_name <- "peso_industrial_provincias.csv"
analista <- "Nicolás Sidicaro"
fuente1 <- 'R221C92' # PBG CEPAL CEP

# carga de fuentes
df_pbg <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()

# armar datos pbg 
seleccionar_industrias_pbg <- df_pbg %>% 
  select(sector_de_actividad_economica) %>% 
  distinct() %>% 
  filter(row_number() > 5 & row_number() < 30) %>% 
  pull(sector_de_actividad_economica)


df_output <- df_pbg %>% 
  filter(sector_de_actividad_economica != 'Total sectores') %>% 
  mutate(industria = if_else(sector_de_actividad_economica %in% seleccionar_industrias_pbg,'industria','otro')) %>% 
  filter(provincia != 'No distribuido') %>% 
  group_by(anio,provincia,provincia_id,industria) %>% 
  summarize(vab_pb = sum(vab_pb)) %>% 
  ungroup() %>% 
  group_by(anio,provincia) %>% 
  mutate(prop_industrial = vab_pb / sum(vab_pb)) %>% 
  ungroup() %>% 
  filter(industria == 'industria') %>% 
  select(anio, provincia_id, provincia, prop_industrial)


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico, drive = T) 


df_comparable <- df_output %>% 
  rename(prop = prop_industrial)


pks <- c('anio','provincia_id')

comparacion <- argendataR::comparar_outputs(
  df = df_comparable,
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
    unidad = list("poblacion" = "unidades", "share" = "porcentaje"))


output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "main")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "main")
