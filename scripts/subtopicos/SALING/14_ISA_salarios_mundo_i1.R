#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection

limpiar_temps()

code_name <- '14_ISA_salarios_mundo_i1.R'
subtopico <- 'SALING'
output_name <- 'ISA_salarios_mundo_i1.csv'
fuente1 <- 'R179C0' # WTO Exportaciones servicios por país, subproducto, anio.

geo_front <- argendataR::get_nomenclador_geografico_front() %>% 
  select(geocodigo, pais_nombre = name_long)

df_output <- readxl::read_excel(argendataR::get_raw_path(fuente1))
colnames(df_output) <- str_replace(colnames(df_output), " ", "") 
df_output <- df_output %>% 
  mutate(
    iso3 = case_when(pais == "ELS" ~ "SLV",
                     pais == "PAR" ~ "PRY",
                     TRUE ~ pais
                     )
  ) %>% 
  select(-pais) %>% 
  left_join(geo_front, join_by(iso3 == geocodigo)) %>% 
  select(iso3, pais_nombre, salariohorario)
  

df_anterior <- argendataR::descargar_output(nombre = output_name, subtopico = subtopico, entrega_subtopico = "primera_entrega") %>% 
  rename(iso3 = pais)

#-- Controlar Output ----

comparacion <- argendataR::comparar_outputs(
  df_output,
  df_anterior,
  pk = c('iso3'),
  drop_joined_df = F
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
  dplyr::filter(dataset_archivo == output_name) %>% 
  distinct(variable_nombre, descripcion) 



# Guardo en una variable las columnas del output que queremos escribir
output_cols <- names(df_output) # lo puedo generar así si tengo df_output


etiquetas_nuevas <- data.frame(
  variable_nombre = c("pais_nombre", 
                      "iso3"),
  descripcion = c("Nombre de país",
                  "Código de país ISO 3166-1 alpha-3")
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



df_output %>%
  argendataR::write_output(
    output_name = output_name,
    aclaraciones = "Ingreso laboral horario en América Latina. 2021 o año más reciente.",
    subtopico = subtopico,
    fuentes = colectar_fuentes(),
    analista = "",
    pk =  c('iso3'),
    control = comparacion, 
    es_serie_tiempo = F,
    nivel_agregacion ="paises",
    descripcion_columnas = descripcion,
    etiquetas_indicadores = list('salariohorario' = 'Valor promedio por país del ingreso laboral por hora de la ocupación principal de todos los trabajadores expresados en dólares a paridad de poder de compra (PPP 2011)'),
    unidades = list('salariohorario' = 'unidades')
  )

nombre_archivo <- tools::file_path_sans_ext(output_name)
mandar_data(paste0(nombre_archivo, ".csv"), subtopico = subtopico, branch = "dev")
mandar_data(paste0(nombre_archivo, ".json"), subtopico = subtopico,  branch = "dev")
