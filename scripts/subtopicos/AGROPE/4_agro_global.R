################################################################################
##                              Dataset: nombre                               ##
################################################################################


#limpio la memoria
rm( list=ls() )  #Borro todos los objetos
gc()   #Garbage Collection


subtopico <- "AGROPE"
output_name <- "agro_global"
analista = "Franco Antonio Mendoza"
fuente1 <- "R295C0" # The World Bank. Agriculture, forestry, and fishing, value added (% of GDP)



df_wb <- readr::read_csv(argendataR::get_raw_path(fuente1)) 

geonomenclador <- argendataR::get_nomenclador_geografico() %>% 
  select(iso3c = codigo_fundar, pais = desc_fundar, nivel_agregacion)



df_output <- df_wb %>% 
  mutate(
    iso3c = case_when(
      country == "High income" ~ "HIC",
      country == "Low income" ~ "LIC",
      country == "Lower middle income" ~ "LMC",
      country == "Upper middle income" ~ "UMC",
      TRUE ~ iso3c
    )
  ) %>% 
  left_join(geonomenclador, join_by(iso3c)) %>% 
  select(anio = year, iso3c, pais, nivel_agregacion, va_agro_sobre_pbi = `NV.AGR.TOTL.ZS`) %>% 
  drop_na(va_agro_sobre_pbi) %>% 
  arrange(iso3c, anio) %>% 
  dplyr::filter(!is.na(iso3c))

  


df_anterior <- argendataR::descargar_output(nombre = output_name,
                                            subtopico = subtopico,
                                            entrega_subtopico = "primera_entrega") 

comparacion <- argendataR::comparar_outputs(
  df_anterior = df_anterior,
  df = df_output,
  nombre = output_name,
  pk = c("anio", 'iso3c'), 
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
  variable_nombre = c("nivel_agregacion"),
  descripcion = c("Indicador de si el registro se refiere a un 'pais' o una 'agregacion' de países")
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
    pk = c("anio",'iso3c'),
    es_serie_tiempo = T,
    control = comparacion, 
    columna_geo_referencia = 'iso3c',
    columna_indice_tiempo = 'anio',
    descripcion_columnas = descripcion,
    unidades = list("va_agro_sobre_pbi" = "porcentaje")
  )
