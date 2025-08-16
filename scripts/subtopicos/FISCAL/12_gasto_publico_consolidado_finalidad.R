# Limpio la memoria
rm(list = ls())  # Borro todos los objetos
gc()  # Garbage Collection

# Defino variables
subtopico <- "FISCAL"
output_name <- "gasto_publico_consolidado_finalidad.csv"
analista <- "María Fernanda Villafañe & Micaela Fernandez Erlauer"


fuente1 <- 'R325C200' # Gasto Público consolidado % PIB


df_mecon <- argendataR::get_clean_path(fuente1) %>% 
  arrow::read_parquet()
  

df_output <- df_mecon %>% 
  dplyr::filter(codigo %in% c("1.1", "1.2", "1.3", "1.4")) %>% 
  mutate(finalidad = case_when(
    codigo == "1.1" ~ "Funcionamiento del estado",
    codigo == "1.2" ~ "Gasto público social",
    codigo == "1.3" ~ "Gasto público en servicios económicos",
    codigo == "1.4" ~ "Servicios de la deuda pública",
    TRUE ~ NA_character_
  )) %>% 
  select(anio, finalidad, gasto_publico_consolidado = valores)

comparable_df <- df_output %>% 
  mutate(finalidad = finalidad %>% janitor::make_clean_names(., allow_dupes = T))


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            drive = T) %>% 
  mutate(anio = as.integer(anio))

comparacion <- argendataR::comparar_outputs(
  df = comparable_df,
  df_anterior = df_anterior,
  nombre = output_name,
  pk = c("anio", "finalidad")
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
    pk = c("anio", "finalidad"),
    descripcion_columnas = descripcion,
    unidades = list("gasto_publico_consolidado" = "porcentaje")
  )

output_name <- gsub("\\.csv", "", output_name)
mandar_data(paste0(output_name, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(output_name, ".json"), subtopico = subtopico,  branch = "dev")
